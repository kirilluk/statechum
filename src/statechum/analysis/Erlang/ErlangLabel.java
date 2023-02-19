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

import com.ericsson.otp.erlang.*;
import statechum.Configuration;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.Erlang.Signatures.Signature;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import java.math.BigInteger;
import java.util.*;

import static statechum.analysis.Erlang.ErlangLabel.ErlangMap.ParseState.EXPECT_SEPARATOR;

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
	/**
	 * A function might be called wibble:handle_call/3 but we have to use the
	 * name of "call" when making a call to Erlang.
	 */
	public final String callName;

	/**
	 * Denotes the name of the function which is known to the module but not
	 * stored in this label.
	 */
	public static final String missingFunction = "?F()";

	protected String buildFunctionSignatureAsString() {
		// {File, LineNo, F, A,fun_to_Statechum(erl_types:t_fun(ArgType,
		// RetType),Info#info.recMap)}
		StringBuffer resultHolder = new StringBuffer();
		resultHolder.append('{');
		if (function == null) {
			resultHolder.append(missingFunction);
		} else {
			resultHolder.append(function.toErlangTerm());
			resultHolder.append(",");
			resultHolder.append(arity);
		}
		resultHolder.append(',');
		ErlangLabel.ErlangQuotedAtom.getSingleton()
				.dump(callName, resultHolder);
		resultHolder.append(',');
		resultHolder.append(dumpErlangObject(input));
		if (expectedOutput != null) {
			resultHolder.append(',');
			resultHolder.append(dumpErlangObject(expectedOutput));
		}
		resultHolder.append('}');
		return resultHolder.toString();
	}

	@Override
	public String toString() {
		String result = callName + ", " + input;
		// (function == null?missingFunction:function.toString())+"," + input;
		if (expectedOutput != null) {
			result = result + " ," + expectedOutput;
		}
		return "{" + result + "}";
	}

	private final String alphaNum;

	@Override
	public String toErlangTerm() {
		return alphaNum;
	}

	public static String dumpErlangObject(OtpErlangObject obj) {
		StringBuffer buffer = new StringBuffer();
		@SuppressWarnings("rawtypes")
		Class cls = obj.getClass();
		ErlangParserComponent dumper = classToDumper.get(cls);
		while (dumper == null) {
			cls = cls.getSuperclass();
			if (cls == null)
				throw new IllegalArgumentException(
						"cannot dump object of type " + obj.getClass());

			dumper = classToDumper.get(cls);
		}
		dumper.dump(obj, buffer);
		return buffer.toString();
	}

	public ErlangLabel(FuncSignature operator, String shortName,
			OtpErlangObject inputArgs) {
		this(operator, shortName, inputArgs, null);
	}

	public ErlangLabel(FuncSignature operator, String shortName,
			OtpErlangObject inputArgs, OtpErlangObject expectedOutputArgs) {
		super(
				// if there is no valid function involved, this label cannot be passed to Erlang.
				operator == null? new OtpErlangObject[]{}:
				expectedOutputArgs == null ? new OtpErlangObject[] {
				// if no outputs, generate a function/input pair
				new OtpErlangAtom(operator.getName()), inputArgs }
				: new OtpErlangObject[] { 
				// with an output, this will be a function/input/output triple.
				new OtpErlangAtom(operator.getName()),inputArgs, expectedOutputArgs });
		arity = expectedOutputArgs == null ? 2 : 3;
		function = operator;
		callName = shortName;
		input = inputArgs;
		expectedOutput = expectedOutputArgs;
		alphaNum = buildFunctionSignatureAsString();
		if (function != null)
			function.typeCompatible(this);
	}

	@Override
	public int compareTo(Label other) {
		if (!(other instanceof ErlangLabel)) {
			throw new IllegalArgumentException(
					"Comparing an ErlangLabel to something thats not an ErlangLabel");
		}

		return toErlangTerm().compareTo(other.toErlangTerm());
	}

	/*
	 * (non-Javadoc)
	 * 
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof  Label))
			return false;
		return toErlangTerm().equals(((Label) obj).toErlangTerm());
	}

	/** Implemented by differet components of Erlang<->text parser. */
	public interface ErlangParserComponent {
		/**
		 * Turns the text of this component into text which can be subsequently
		 * parsed back. For instance, string are quoted and so are atoms, hence
		 * no type conversion is necessary.
		 */
		void dump(OtpErlangObject obj, StringBuffer resultHolder);

		/**
		 * Parses text using the provided parser and returns an Erlang term
		 * which corresponds to what lexer observes.
		 * 
		 * @param lex
		 *            lexer to use
		 * @return Erlang term
		 */
		OtpErlangObject parseObject(Lexer lex);
	}
	public static class ErlangMap implements ErlangParserComponent {
		private static final ErlangMap singleton = new ErlangMap();

		public static ErlangMap getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangMap tuple = (OtpErlangMap) arg;

			resultHolder.append("#{");
			boolean first = true;
			for (Map.Entry<OtpErlangObject, OtpErlangObject> pair : tuple.entrySet()) {
				if (!first)
					resultHolder.append(',');
				else
					first = false;
				classToDumper.get(pair.getKey().getClass()).dump(pair.getKey(), resultHolder);
				resultHolder.append(" => ");
				classToDumper.get(pair.getValue().getClass()).dump(pair.getValue(), resultHolder);
			}
			resultHolder.append("}");
		}
		enum ParseState {
				EXPECT_FIRST("first element of pair"), EXPECT_SEPARATOR("=>"), EXPECT_SECOND("second element of pair"), EXPECT_COMMA(",");
			private final String token;

			public String getToken() {
				return token;
			}

			ParseState(String tok) {
				token = tok;
			}
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlMapBegin;

			List<OtpErlangObject> mapKeys = new ArrayList<>(), mapValues = new ArrayList<>();

			OtpErlangObject firstElementOfPair = null;
			// Parser state.
			ParseState parseState = ParseState.EXPECT_FIRST;
			boolean pullNextToken = true;// this will normally be true - it is false where we called ourselves recursively to
			 	// parse a chunk of data and the parser stopped at the end of that data. This means we need that ending token
				// to continue parsing thus should not grab another token (effectively discarding the current one).
			int currentMatch = lexer.getMatchType();
			while (currentMatch != erlTupleEnd) {// parsing ErlangMap that is expected to end via erlTupleEnd
				if (currentMatch < 0)
					throw new IllegalArgumentException(
							"unexpected end of map");

				switch (currentMatch) {
					case erlMapSep:
						if (parseState != EXPECT_SEPARATOR)
							throw new IllegalArgumentException(
									"expecting => in parsing map, looking at "
											+ lexer.getMatch());
						parseState = ParseState.EXPECT_SECOND;
						break;
					case erlMapBegin:
					case erlTupleBegin:
					case erlListBegin:
					case erlAtomQuote:
					case erlBitStrBegin:
					case erlString:
						if (parseState == ParseState.EXPECT_FIRST) {
							assert firstElementOfPair == null;
							firstElementOfPair = tokenToParser.get(currentMatch).parseObject(lexer);
							parseState = EXPECT_SEPARATOR;
						} else
						if (parseState == ParseState.EXPECT_SECOND) {
							assert firstElementOfPair != null;
							mapKeys.add(firstElementOfPair);mapValues.add(tokenToParser.get(currentMatch).parseObject(lexer));
							parseState = ParseState.EXPECT_COMMA;firstElementOfPair = null;
						} else
							throw new IllegalArgumentException(
									"expecting "+parseState.getToken()+" in parsing map, looking at "
											+ lexer.getMatch());
						break;
					case erlPositiveNumber:
					case erlNegativeNumber:
					case erlDot:
					case erlE:
					case erlText:
						if (parseState == ParseState.EXPECT_FIRST) {
							assert firstElementOfPair == null;
							firstElementOfPair = tokenToParser.get(currentMatch).parseObject(lexer);
							parseState = EXPECT_SEPARATOR;
						} else
						if (parseState == ParseState.EXPECT_SECOND) {
							assert firstElementOfPair != null;
							mapKeys.add(firstElementOfPair);mapValues.add(tokenToParser.get(currentMatch).parseObject(lexer));
							parseState = ParseState.EXPECT_COMMA;firstElementOfPair = null;
						} else
							throw new IllegalArgumentException(
									"expecting "+parseState.getToken()+" in parsing map, looking at "
											+ lexer.getMatch());

						pullNextToken = false;
						break;
					case erlComma:
						if (parseState != ParseState.EXPECT_COMMA)
							throw new IllegalArgumentException(
									"unexpected token in parsing map, looking at "
											+ lexer.getMatch());
						parseState = ParseState.EXPECT_FIRST;
						break;
					default:
						throw new IllegalArgumentException("invalid token type "
								+ currentMatch + " in parsing map, looking at "
								+ lexer.getMatch()+ " so far parsed: "+mapKeys+" keys");
				}
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else {// use the last token but pull the next one next time
					// 'round
					currentMatch = lexer.getLastMatchType();
					pullNextToken = true;
				}
			}
			return new OtpErlangMap(mapKeys.toArray(new OtpErlangObject[0]), mapValues.toArray(new OtpErlangObject[0]));
		}

	}

	public static class ErlangTuple implements ErlangParserComponent {
		private static final ErlangTuple singleton = new ErlangTuple();

		public static ErlangTuple getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangTuple tuple = (OtpErlangTuple) arg;

			resultHolder.append("{");
			boolean first = true;
			for (OtpErlangObject obj : tuple.elements()) {
				if (!first)
					resultHolder.append(',');
				else
					first = false;
				classToDumper.get(obj.getClass()).dump(obj, resultHolder);
			}
			resultHolder.append("}");
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlTupleBegin;

			List<OtpErlangObject> tupleComponents = new LinkedList<>();

			// Parser state.
			boolean expectComma = false, pullNextToken = true;
			int currentMatch = lexer.getMatchType();
			while (currentMatch != erlTupleEnd) {// parsing ErlangTuple
				if (currentMatch < 0)
					throw new IllegalArgumentException(
							"unexpected end of tuple");

				switch (currentMatch) {
				case erlMapBegin:
				case erlTupleBegin:
				case erlListBegin:
				case erlAtomQuote:
				case erlBitStrBegin:
				case erlString:
					if (expectComma)
						throw new IllegalArgumentException(
								"expecting comma in parsing tuple, looking at "
										+ lexer.getMatch());
					tupleComponents.add(tokenToParser.get(currentMatch)
							.parseObject(lexer));
					expectComma = true;
					break;
				case erlMapSep:
					throw new IllegalArgumentException(
							"unexpected => in parsing tuple, looking at "	+ lexer.getMatch());
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlDot:
				case erlE:
				case erlText:
					if (expectComma)
						throw new IllegalArgumentException(
								"expecting comma in parsing tuple, looking at "+lexer.getMatch());
					tupleComponents.add(tokenToParser.get(currentMatch)
							.parseObject(lexer));
					expectComma = true;
					pullNextToken = false;
					break;
				case erlComma:
					if (!expectComma)
						throw new IllegalArgumentException(
								"unexpected comma in parsing tuple, looking at "
										+ lexer.getMatch());
					expectComma = false;
					break;
				default:
					throw new IllegalArgumentException("invalid token type "
							+ currentMatch + " in parsing tuple, looking at "
							+ lexer.getMatch()+ " so far parsed: "+tupleComponents);
				}
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else {// use the last token but pull the next one next time
						// 'round
					currentMatch = lexer.getLastMatchType();
					pullNextToken = true;
				}
			}
			return new OtpErlangTuple(
					tupleComponents.toArray(new OtpErlangObject[0]));
		}

	}

	public static class ErlangList implements ErlangParserComponent {
		private static final ErlangList singleton = new ErlangList();

		public static ErlangList getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangList list = (OtpErlangList) arg;

			resultHolder.append("[");
			boolean first = true;
			for (OtpErlangObject obj : list.elements()) {
				if (!first)
					resultHolder.append(',');
				else
					first = false;
				classToDumper.get(obj.getClass()).dump(obj, resultHolder);
			}
			
			if (!list.isProper())
			{
				OtpErlangObject lastTail = list.getLastTail();resultHolder.append(" | ");
				classToDumper.get(lastTail.getClass()).dump(lastTail, resultHolder);
			}
			resultHolder.append("]");
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlListBegin;

			List<OtpErlangObject> listComponents = new LinkedList<>();
			OtpErlangObject tail = null;
			
			OtpErlangObject nextElement = null;// temporary variable.
			// Parser state.
			boolean expectComma = false, pullNextToken = true, parsingTail = false;
			int currentMatch = lexer.getMatchType();
			while (currentMatch != erlListEnd) {// parsing ErlangList
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of list");

				switch (currentMatch) {
				case erlMapBegin:
				case erlTupleBegin:
				case erlListBegin:
				case erlAtomQuote:
				case erlBitStrBegin:
				case erlString:
					if (expectComma)
						throw new IllegalArgumentException("expecting comma in parsing list, looking at "+lexer.getText());
					if (tail != null)
						throw new IllegalArgumentException("an expression past the end of a tail of an improper list, matched "+
								lexer.getMatch()+" was parsing "+lexer.getText());

					nextElement = tokenToParser.get(currentMatch).parseObject(lexer);
					if (parsingTail)
					{
						tail = nextElement;
					}
					else
						listComponents.add(nextElement);
					
					expectComma = true;
					break;
				case erlMapSep:
					throw new IllegalArgumentException(
							"unexpected => in parsing list, looking at "	+ lexer.getMatch());
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlDot:
				case erlE:
				case erlText:
					if (expectComma)
						throw new IllegalArgumentException(
								"expecting comma in parsing list but matched "+lexer.getMatch()+" was parsing "+lexer.getText());
					if (tail != null)
						throw new IllegalArgumentException("an expression past the end of a tail of an improper list, matched "+
								lexer.getMatch()+" was parsing "+lexer.getText());
					nextElement = tokenToParser.get(currentMatch).parseObject(lexer);
					// the above terminated having eaten a token past the element it is supposed to have parsed.
					pullNextToken = false;

					if (parsingTail)
						tail = nextElement;
					else
						listComponents.add(nextElement);
					
					expectComma = true;
					break;
				case erlBar:
					parsingTail = true;
					if (!expectComma || tail != null)
						throw new IllegalArgumentException(
								"unexpected bar in parsing list, looking at "	+ lexer.getMatch());
					expectComma = false;
					break;
				case erlComma:
					if (!expectComma || tail != null)
						throw new IllegalArgumentException(
								"unexpected comma in parsing list, looking at "	+ lexer.getMatch());
					expectComma = false;
					break;
				default:
					throw new IllegalArgumentException("invalid token type " + currentMatch + " in parsing list, looking at "+ lexer.getMatch());
				}
				if (pullNextToken)
					currentMatch = lexer.getMatchType();
				else {// use the last token but pull the next one next time
					  // 'round
					currentMatch = lexer.getLastMatchType();
					pullNextToken = true;
				}
			}
			
			if (parsingTail && tail == null)
				throw new IllegalArgumentException("missing tail in improper list");
			
			if (tail != null)
			{
				// The special case is where the end is a string which we have to turn into a list
				// in order to merge it into the current list.
				if (tail instanceof OtpErlangString)
					tail = Signature.stringToList(tail);
				
				if (tail instanceof OtpErlangList)
				{// merge this into the current list.
					OtpErlangList tailAsList = (OtpErlangList)tail;
					for(int i=0;i<tailAsList.arity();++i)
						listComponents.add(tailAsList.elementAt(i));
					tail = tailAsList.getLastTail();
				}
			}
			
			OtpErlangList outcome = null;
			try {
				outcome = new OtpErlangList(listComponents.toArray(new OtpErlangObject[0]),tail);
			} catch (OtpErlangException e) {
				Helper.throwUnchecked("failed to build a list", e);
			}
			return outcome;
		}
	}

	public static class ErlangQuotedAtom implements ErlangParserComponent {
		private static final ErlangQuotedAtom singleton = new ErlangQuotedAtom();

		public static ErlangQuotedAtom getSingleton() {
			return singleton;
		}

		protected static final Set<Character> whatToQuoteForAtom = new HashSet<Character>();

		static {
			for (char ch : new char[] { '\'', '\\', '\n', '\r' })
				whatToQuoteForAtom.add(ch);
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangAtom atom = (OtpErlangAtom) arg;
			resultHolder.append('\'');
			stringToText(atom.atomValue(), whatToQuoteForAtom, resultHolder);
			resultHolder.append('\'');
		}

		public void dump(String arg, StringBuffer resultHolder) {
			resultHolder.append('\'');
			stringToText(arg, whatToQuoteForAtom, resultHolder);
			resultHolder.append('\'');
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlAtomQuote;
			StringBuilder atomText = new StringBuilder();

			// parser state
			boolean expectedChar = false, finished = false;

			while (!finished) {
				int currentMatch = lexer.getMatchType();
				if (currentMatch < 0)
					throw new IllegalArgumentException("unexpected end of atom");

				switch (currentMatch) {// parsing quoted ErlangAtom
				case erlMapBegin:
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
				case erlPlus:
				case erlSlash:
				case erlDot:
				case erlE:
				case erlSpaces:
				case erlComma:
				case erlMapSep:
				case erlBar:
					atomText.append(lexer.getMatch());
					expectedChar = false;
					break;
				case erlBackslash:
					if (expectedChar) {// this char will be quoted
						atomText.append(lexer.getMatch());
						expectedChar = false;
					} else
						expectedChar = true;
					break;
				case erlAtomQuote:
					if (expectedChar) {// this char will be quoted
						atomText.append(lexer.getMatch());
						expectedChar = false;
					} else
						finished = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					if (expectedChar)
						// first character quoted - we do not support special
						// chars in atoms because they are not supposed to be
						// quoted anyway.
						throw new IllegalArgumentException(
								"atom parser: character "
										+ lexer.getMatch().charAt(0)
										+ " is not supposed to be prefixed by backslash");

					atomText.append(lexer.getMatch());
					break;
				default:
					throw new IllegalArgumentException("invalid token type "
							+ currentMatch + " in parsing atom, looking at "
							+ lexer.getMatch());
				}
			}

			return new OtpErlangAtom(atomText.toString());
		}
	}

	/** Parsing is done by the UnquotedAtom class since booleans are unquoted. */
	public static class ErlangBoolean implements ErlangParserComponent {
		private static final ErlangBoolean singleton = new ErlangBoolean();

		public static ErlangBoolean getSingleton() {
			return singleton;
		}

		public static final String True = "true", False = "false";

		@Override
		public void dump(OtpErlangObject obj, StringBuffer resultHolder) {
			if (((OtpErlangBoolean) obj).booleanValue())
				resultHolder.append(True);
			else
				resultHolder.append(False);
		}

		@Override
		public OtpErlangObject parseObject(@SuppressWarnings("unused") Lexer lex) {
			throw new UnsupportedOperationException(
					"parsing is supposed to be done by ErlangUnquotedAtom");
		}
	}

	public static class ErlangUnquotedAtom extends ErlangQuotedAtom {
		private static final ErlangUnquotedAtom singleton = new ErlangUnquotedAtom();

		public static ErlangUnquotedAtom getSingleton() {
			return singleton;
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlText
					|| lexer.getLastMatchType() == erlE
					|| lexer.getLastMatchType() == erlDot;
			StringBuilder atomText = new StringBuilder();

			// parser state
			boolean finished = false, firstToken = true;
			int currentMatch = lexer.getLastMatchType();
			while (!finished && currentMatch >= 0) {
				switch (currentMatch) {// parsing Unquoted ErlangAtom
				case erlString:
				case erlAtomQuote:
				case erlBackslash:
				case erlGT: // Erlang interprets statements with these as
							// expressions and attempts to evaluate them, we
							// simply ban 'em
				case erlLT:// Erlang interprets statements with these as
							// expressions and attempts to evaluate them, we
							// simply ban 'em
				case erlCol:
				case erlMinus:
				case erlPlus:
				case erlSlash:
					throw new IllegalArgumentException("unquoted atom parser: "
							+ lexer.getMatch()
							+ " is never allowed in an atom without quotes. Text in atom so far: "+atomText);
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlE:
				case erlText:
					atomText.append(lexer.getMatch());
					break;
				case erlDot:
					if (firstToken)
						throw new IllegalArgumentException(
								"unquoted atom cannot start with a dot which is ignored by the actual Erlang ");
					break;
				case erlBar:
				case erlMapBegin:
				case erlTupleBegin:
				case erlListBegin:
				case erlTupleEnd:
				case erlListEnd:
				case erlBitStrBegin:
				case erlBitStrEnd:
				case erlSpaces:
				case erlMapSep:
				case erlComma:
					finished = true;
					break;
				default:
					throw new IllegalArgumentException("invalid token type "
							+ currentMatch
							+ " in parsing unquoted atom, looking at "
							+ lexer.getMatch());
				}
				firstToken = false;
				if (!finished)
					currentMatch = lexer.getMatchType();
			}
			assert atomText.length() > 0;

			OtpErlangObject outcome = null;
			if (atomText.toString().equals(ErlangBoolean.True))
				outcome = new OtpErlangBoolean(true);
			else if (atomText.toString().equals(ErlangBoolean.False))
				outcome = new OtpErlangBoolean(false);
			else
				outcome = new OtpErlangAtom(atomText.toString());

			return outcome;
		}
	}

	public static class ErlangString implements ErlangParserComponent {
		private static final ErlangString singleton = new ErlangString();

		public static ErlangString getSingleton() {
			return singleton;
		}

		protected static final Set<Character> whatToQuoteForString = new HashSet<Character>();

		static {// perhaps make these if ( !Character.isAlphabetic(ch) && !Character.isDigit(ch) && ch != ' ')
			for (char ch : new char[] { '\"', '\\', '\n', '\r' })
				whatToQuoteForString.add(ch);
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangString atom = (OtpErlangString) arg;
			dump(atom.stringValue(), resultHolder);
		}

		/** Similar to the above but for normal strings. */
		public void dump(String str, StringBuffer resultHolder) {
			resultHolder.append('\"');
			stringToText(str, whatToQuoteForString, resultHolder);
			resultHolder.append('\"');
		}

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlString;
			StringBuilder stringText = new StringBuilder();

			// parser state
			boolean expectedChar = false, finished = false;

			while (!finished) {
				int currentMatch = lexer.getMatchType();
				if (currentMatch < 0)
					throw new IllegalArgumentException(
							"unexpected end of string");

				switch (currentMatch) {// parsing ErlangString
				case erlMapBegin:
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
				case erlPlus:
				case erlSlash:
				case erlDot:
				case erlE:
				case erlComma:
				case erlMapSep:
				case erlBar:
				case erlSpaces:
					stringText.append(lexer.getMatch());
					expectedChar = false;
					break;
				case erlBackslash:
					if (expectedChar) {// this char will be quoted
						stringText.append(lexer.getMatch());
						expectedChar = false;
					} else
						expectedChar = true;
					break;
				case erlString:
					if (expectedChar) {// this char will be quoted
						stringText.append(lexer.getMatch());
						expectedChar = false;
					} else
						finished = true;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
				case erlText:
					if (expectedChar)
						// first character quoted - we do not support special
						// chars in string because they are not supposed to be
						// quoted anyway.
						throw new IllegalArgumentException(
								"string parser: character "
										+ lexer.getMatch().charAt(0)
										+ " is not supposed to be prefixed by backslash");

					stringText.append(lexer.getMatch());
					break;
				default:
					throw new IllegalArgumentException("invalid token type "
							+ currentMatch + " in parsing tuple, looking at "
							+ lexer.getMatch());
				}
			}

			return new OtpErlangString(stringText.toString());
		}
	}

	public static class ErlangBitString implements ErlangParserComponent {
		private static final ErlangBitString singleton = new ErlangBitString();

		public static ErlangBitString getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject obj, StringBuffer resultHolder) {
			OtpErlangBitstr bitStr = (OtpErlangBitstr) obj;
			resultHolder.append("<< ");
			byte[] bitStringData = bitStr.binaryValue();
			if (bitStr.size() != bitStringData.length)
				throw new IllegalArgumentException(
						"the number of bits in the string is not divisible by 8, conversion cannot be accurately performed");
			boolean first = true;
			for (byte b : bitStringData) {
				if (!first)
					resultHolder.append(", ");
				else
					first = false;
				resultHolder.append(b & 0xff);
			}
			resultHolder.append(">>");
		}

		public enum ParseState {
			NUM_OR_END, SCOLON_COMMA_END, SIZE, SLASH_COMMA_END, TYPE, MINUS_COMMA_END, UCOLON, UNIT, COMMA_END, NUM, FINISHED
		}

		public static final Set<String> typeStringsIgnored;

		static {
			Set<String> strings = new TreeSet<String>();
			strings.add("integer");
			typeStringsIgnored = Collections.unmodifiableSet(strings);
		}

		protected class SegmentParameters {
			// default values
			int unit = 1, size = 8;
			boolean littleEndian = false;

			final BigInteger number;
			int lengthOfNumber;

			/** Only used to detect inconsistent type declarations. */
			boolean isSigned = false, isUnsigned = false, isBig = false,
					isLittle = false;

			public SegmentParameters(String text) {
				number = new BigInteger(text);
				lengthOfNumber = number.bitCount();
			}

			/**
			 * Handles all type strings exceptin Unit which requires parser
			 * assistance.
			 */
			public void processTypeAttribute(String type) {
				if (type.equals("little")) {
					if (isBig)
						throw new IllegalArgumentException(
								"type mismatched: already big-endian");
					isLittle = true;
					littleEndian = true;
				} else if (type.equals("big")) {
					if (isLittle)
						throw new IllegalArgumentException(
								"type mismatched: already little-endian");
					isBig = true;
					littleEndian = false;
				} else if (type.equals("signed")) {
					if (isUnsigned)
						throw new IllegalArgumentException(
								"type mismatched: already unsigned");
					isSigned = true;
				} else if (type.equals("unsigned")) {
					if (isSigned)
						throw new IllegalArgumentException(
								"type mismatched: already signed");
					isUnsigned = true;
				} else if (!typeStringsIgnored.contains(type))
					throw new IllegalArgumentException(
							"unknown type specifier " + type);
			}

			/**
			 * Given the byte from our big number and the length of the big
			 * number, it updates our last byte, data and returns the remaining
			 * length of the big number.
			 * 
			 * @param byteToAdd
			 *            byte to store
			 * @param lenArg
			 *            current length of big number
			 * @return the remaining length of big number
			 */
			protected int addBigEndianByte(byte byteToAdd, int lenArg) {
				int len = lenArg;
				int bitsUsedInLeftmostByte = len % 8 == 0 ? 8 : len % 8;
				int bitsSpareMinusBitsNeeded = (8 - (totalLen % 8))
						- bitsUsedInLeftmostByte;
				if (bitsSpareMinusBitsNeeded < 0) 
				{// last byte: UUUUU...
				 // new byte: ...XXXXXX,
				 // bitsSpareMinusBitsNeeded
				 // is what will not fit in
				 // the last byte
					int rightShift = -bitsSpareMinusBitsNeeded;
					// Masking is from
					// http://stackoverflow.com/questions/11088/what-is-the-best-way-to-work-around-the-fact-that-all-java-bytes-are-signed
					lastByte |= (byte) ((byteToAdd >> rightShift) & ((1 << (8 - rightShift)) - 1));
					data.add(lastByte);
					lastByte = (byte) (byteToAdd << (8 - rightShift));// make the last byte big-endian

					int bitsAdded = bitsUsedInLeftmostByte;
					totalLen += bitsAdded;
					len -= bitsAdded;
				} else {// enough space in the last byte
						// last byte: UUU.....
						// new byte: ......XX, bitsSpareMinusBitsNeeded is what
						// will remain in the last byte
					lastByte |= (byte) (byteToAdd << bitsSpareMinusBitsNeeded);
					int bitsAdded = bitsUsedInLeftmostByte;
					totalLen += bitsAdded;
					len -= bitsAdded;
					if (totalLen % 8 == 0)// last byte is full
					{
						data.add(lastByte);
						lastByte = 0;
					}
				}

				return len;
			}

			/**
			 * Appends the byte representation of the data in this segment to
			 * the collection.
			 */
			public void appendNumbers() {
				// data in the last byte is always big-endian
				int len = unit * size;
				if (len == 0)
					return;// empty number, do nothing
				if (lengthOfNumber > len)
					throw new IllegalArgumentException("Number with "
							+ lengthOfNumber + " bits will not fit inside "
							+ len + "-bit segment");
				BigInteger num = number.mod(new BigInteger(new byte[] { 1 })
						.shiftLeft(len));
				byte[] integerData = new byte[len % 8 > 0 ? 1 + len / 8
						: len / 8];

				// the number of bytes in this array may be arbitrary, possibly
				// greater or lower
				// than integerData.length, copying ensures that we have as many
				// bytes there
				// as we expect.
				byte[] numberData = num.toByteArray();
				for (int i = 0; i < Math.min(integerData.length,
						numberData.length); ++i)
					integerData[integerData.length - 1 - i] = numberData[numberData.length
							- 1 - i];

				if (!littleEndian) {// dumping big-endian
					for (byte currentByte : integerData) {
						len = addBigEndianByte(currentByte, len);
					}
				} else {// little-endian case
					for (int i = integerData.length - 1; i > 0; --i) 
					{// not enough (or just enough) space to fit in the last
					 // byte (we are trying to fit an entire byte in it).
						byte currentByte = integerData[i];
						int bitsSpare = (8 - (totalLen % 8));
						int bitsUsed = 8 - bitsSpare;
						lastByte |= (byte) ((currentByte >> bitsUsed) & ((1 << bitsSpare) - 1));
						data.add(lastByte);
						lastByte = (byte) ((currentByte << bitsSpare));// the spillover goes in the low bits of the next byte - we are little-endian
						totalLen += 8;
						len -= 8;
					}
					// last byte
					byte currentByte = integerData[0];
					addBigEndianByte(currentByte, len);
				}
			}
		}

		List<Byte> data = new LinkedList<Byte>();
		byte lastByte = 0;
		int totalLen = 0;

		@Override
		public OtpErlangObject parseObject(Lexer lexer) 
		{// I need a new instance of this object for each
		 // bit string, hence have to create it here (on demand).
			return new ErlangBitString().parseObjectInternal(lexer);
		}

		public OtpErlangObject parseObjectInternal(Lexer lexer) {
			assert lexer.getLastMatchType() == erlBitStrBegin;

			// parser state
			ParseState state = ParseState.NUM_OR_END;

			SegmentParameters currentSegment = null;
			while (state != ParseState.FINISHED) {
				int currentMatch = lexer.getMatchType();

				if (currentMatch < 0)
					throw new IllegalArgumentException(
							"unexpected end of bit string");

				switch (currentMatch) {// parsing quoted ErlangAtom
				case erlBitStrEnd:
					switch (state) {
					case NUM_OR_END:
					case SCOLON_COMMA_END:
					case SLASH_COMMA_END:
					case MINUS_COMMA_END:
					case COMMA_END:
						if (currentSegment != null)
							currentSegment.appendNumbers();
						if (totalLen % 8 != 0)
							data.add(lastByte);
						state = ParseState.FINISHED;
						break;
					default:
						throw new IllegalArgumentException(
								"BitStr parser: got " + lexer.getMatch()
										+ " in " + state + " state");
					}
					break;
				case erlCol:
					switch (state) {
					case SCOLON_COMMA_END:
						state = ParseState.SIZE;
						break;
					case UCOLON:
						state = ParseState.UNIT;
						break;
					default:
						throw new IllegalArgumentException(
								"BitStr parser: got " + lexer.getMatch()
										+ " in " + state + " state");
					}
					break;
				case erlMinus:
					if (state != ParseState.MINUS_COMMA_END)
						throw new IllegalArgumentException(
								"BitStr parser: dash can only be used after a type, looking at "
										+ lexer.getMatch());
					state = ParseState.TYPE;
					break;
				case erlSpaces:
					break;// ignore this
				case erlComma:
					switch (state) {
					case SCOLON_COMMA_END:
					case SLASH_COMMA_END:
					case MINUS_COMMA_END:
					case COMMA_END:
						state = ParseState.NUM;
						break;
					default:
						throw new IllegalArgumentException(
								"BitStr parser: got " + lexer.getMatch()
										+ " in " + state + " state");
					}
					break;
				case erlSlash:
					if (state != ParseState.SLASH_COMMA_END)
						throw new IllegalArgumentException(
								"BitStr parser: slash can only be used after NUMBER:SIZE declaration, looking at "
										+ lexer.getMatch());
					state = ParseState.TYPE;
					break;
				case erlPositiveNumber:
				case erlNegativeNumber:
					switch (state) {
					case NUM:
					case NUM_OR_END:
						// NUMBER:unit/... reading this NUMBER
						if (currentSegment != null)
							currentSegment.appendNumbers();
						currentSegment = new SegmentParameters(lexer.getMatch()
								.trim());
						state = ParseState.SCOLON_COMMA_END;
						break;
					case SIZE:
						currentSegment.size = Integer.parseInt(lexer.getMatch()
								.trim());
						state = ParseState.SLASH_COMMA_END;
						break;
					case UNIT:
						currentSegment.unit = Integer.parseInt(lexer.getMatch()
								.trim());
						state = ParseState.COMMA_END;
						break;
					default:
						throw new IllegalArgumentException(
								"BitStr parser: got " + lexer.getMatch()
										+ " in " + state + " state");
					}
					break;
				case erlE:
				case erlText:
					if (state != ParseState.TYPE)
						throw new IllegalArgumentException(
								"BitStr parser: type is unexpected in this position, looking at "
										+ lexer.getMatch());
					if (lexer.getMatch().equals("unit"))
						state = ParseState.UCOLON;
					else {
						currentSegment.processTypeAttribute(lexer.getMatch());
						state = ParseState.MINUS_COMMA_END;
					}
					break;
				default:
					throw new IllegalArgumentException("invalid token type "
							+ currentMatch + " in parsing atom, looking at "
							+ lexer.getMatch());
				}
			}

			if (totalLen % 8 != 0)
				throw new IllegalArgumentException(
						"the length of bit string should be divisible by 8");
			byte[] bitData = new byte[data.size()];
			int i = 0;
			for (Byte b : data)
				bitData[i++] = b;
			return new OtpErlangBitstr(bitData);
		}
	}

	public static class ErlangLong implements ErlangParserComponent {
		private static final ErlangLong singleton = new ErlangLong();

		public static ErlangLong getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangLong longValue = (OtpErlangLong) arg;
			resultHolder.append(longValue.longValue());
		}

		private enum ParseState {
			PARSE_DOT, PARSE_SECONDNUM, PARSE_E, PARSE_EXP, PARSE_END, PARSE_FINISHED
		}

		private static final String SPACE = " ";

		public static final int minExponent = 324, maxExponent = 307;

		@Override
		public OtpErlangObject parseObject(Lexer lexer) {
			assert lexer.getLastMatchType() == erlPositiveNumber
					|| lexer.getLastMatchType() == erlNegativeNumber;
			String partA = lexer.getMatch().trim(), partB = null, partExp = null;

			// after a space, we can only expect a separator char, but nothing
			// part of this number.
			ParseState state = lexer.getMatch().endsWith(SPACE) ? ParseState.PARSE_END
					: ParseState.PARSE_DOT;

			while (state != ParseState.PARSE_FINISHED) {
				int currentMatch = lexer.getMatchType();
				if (currentMatch < 0) {
					if (state == ParseState.PARSE_EXP)
						throw new IllegalArgumentException(
								"unexpected end of text");

					state = ParseState.PARSE_FINISHED;
				} else
					switch (currentMatch) {
					case erlPositiveNumber:
						switch (state) {
						case PARSE_SECONDNUM:
							if (lexer.getMatch().startsWith(SPACE))
								throw new IllegalArgumentException(
										"unexpected space, parsing number, looking at "
												+ lexer.getMatch());
							partB = lexer.getMatch().trim();
							if (lexer.getMatch().endsWith(SPACE))
								state = ParseState.PARSE_END;
							state = ParseState.PARSE_E;
							break;
						case PARSE_EXP:
							if (lexer.getMatch().startsWith(SPACE))
								throw new IllegalArgumentException(
										"unexpected space, parsing number, looking at "
												+ lexer.getMatch());
							partExp = lexer.getMatch().trim();
							state = ParseState.PARSE_END;
							break;
						default:
							throw new IllegalArgumentException(
									"invalid token, expected dot, parsing number in state "
											+ state + " looking at "
											+ lexer.getMatch());
						}
						break;
					case erlNegativeNumber:
						switch (state) {
						case PARSE_EXP:
							if (lexer.getMatch().startsWith(SPACE))
								throw new IllegalArgumentException(
										"unexpected space, parsing number, looking at "
												+ lexer.getMatch());
							partExp = lexer.getMatch().trim();
							state = ParseState.PARSE_END;
							break;
						default:
							throw new IllegalArgumentException(
									"expected dot, parsing number in state "
											+ state + " looking at "
											+ lexer.getMatch());
						}
						break;
					case erlDot:
						if (state != ParseState.PARSE_DOT)
							throw new IllegalArgumentException(
									"unexpected dot in state " + state
											+ ", parsing number, looking at "
											+ lexer.getMatch());
						state = ParseState.PARSE_SECONDNUM;
						break;
					case erlE:
						switch (state) {
						case PARSE_DOT:
						case PARSE_E:
						case PARSE_SECONDNUM:
							state = ParseState.PARSE_EXP;
							break;
						default:
							throw new IllegalArgumentException(
									"unexpected exponent in state " + state
											+ ", looking at "
											+ lexer.getMatch());
						}
						break;
					case erlTupleEnd:
					case erlListEnd:
					case erlSpaces:
					case erlComma:
					case erlBar:
					case erlMapSep:
						switch (state) {
						case PARSE_DOT:
						case PARSE_END:
						case PARSE_E:
						case PARSE_SECONDNUM:
							state = ParseState.PARSE_FINISHED;
							break;
						default:
							throw new IllegalArgumentException(
									"invalid token in parsing erlang number, state "
											+ state + ", looking at "
											+ lexer.getMatch());
						}
						break;
					default:
						throw new IllegalArgumentException(
								"invalid token type " + currentMatch
										+ " in parsing erlang number, state "
										+ state + ", looking at "
										+ lexer.getMatch()+ " in "+lexer.getText());
					}
			}
			OtpErlangObject result = null;
			if (partB == null && partExp == null) {// integer/long
				if (partA.startsWith("+"))
					partA = partA.substring(1);
				long outcome = Long.parseLong(partA);
				if (outcome > Integer.MAX_VALUE || outcome < Integer.MIN_VALUE)
					result = new OtpErlangLong(outcome);
				else
					result = new OtpErlangInt((int) outcome);
			} else {// floating - point number.
				String textToParse = partA + "."
						+ (partB == null ? "0" : partB)
						+ (partExp == null ? "" : ("e" + partExp));
				if (partExp != null) {
					long exponent = Long.parseLong(partExp) + partA.length()
							- 1;
					if (exponent < -minExponent || exponent > maxExponent)
						throw new IllegalArgumentException(
								"the number parsed cannot be represented as a double");
				}
				double parsingResult = Double.parseDouble(textToParse);
				if (Double.isInfinite(parsingResult)
						|| Double.isNaN(parsingResult))
					throw new IllegalArgumentException(
							"the number parsed cannot be represented as a double");
				result = new OtpErlangDouble(parsingResult);

			}
			return result;
		}
	}

	public static class ErlangInt implements ErlangParserComponent {
		private ErlangInt() {
		}

		private static final ErlangInt singleton = new ErlangInt();

		public static ErlangInt getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangInt longValue = (OtpErlangInt) arg;
			resultHolder.append(longValue.longValue());
		}

		@Override
		public OtpErlangObject parseObject(
				@SuppressWarnings("unused") Lexer lexer) {
			throw new UnsupportedOperationException(
					"all numbers are supposed to be parsed as long numbers and subsequently converted to int if they are small enough");
		}
	}

	public static class ErlangChar implements ErlangParserComponent {
		private ErlangChar() {
		}

		private static final ErlangChar singleton = new ErlangChar();

		public static ErlangChar getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangChar charValue = (OtpErlangChar) arg;
			resultHolder.append(charValue.longValue());
		}

		@Override
		public OtpErlangObject parseObject(@SuppressWarnings("unused") Lexer lexer) {
			throw new UnsupportedOperationException(
					"number which fit into a character range could be converted to characters, but we do not do this since this is all about guessing");
		}
	}

	public static class ErlangByte implements ErlangParserComponent {
		private ErlangByte() {
		}

		private static final ErlangByte singleton = new ErlangByte();

		public static ErlangByte getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			OtpErlangByte byteValue = (OtpErlangByte) arg;
			resultHolder.append(byteValue.longValue());
		}

		@Override
		public OtpErlangObject parseObject(@SuppressWarnings("unused") Lexer lexer) {
			throw new UnsupportedOperationException(
					"number which fit into a byte range could be converted to characters, but we do not do this since this is all about guessing");
		}
	}

	public static class ErlangDouble implements ErlangParserComponent {
		private ErlangDouble() {
		}

		private static final ErlangDouble singleton = new ErlangDouble();

		public static ErlangDouble getSingleton() {
			return singleton;
		}

		@Override
		public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
			assert (arg instanceof OtpErlangDouble)
					|| (arg instanceof OtpErlangFloat);
			if (arg instanceof OtpErlangDouble) {
				OtpErlangDouble doubleValue = (OtpErlangDouble) arg;
				resultHolder.append(doubleValue.doubleValue());
			} else if (arg instanceof OtpErlangFloat) {
				OtpErlangDouble doubleValue = (OtpErlangDouble) arg;
				resultHolder.append(doubleValue.doubleValue());
			}
		}

		@Override
		public OtpErlangObject parseObject(
				@SuppressWarnings("unused") Lexer lexer) {
			throw new UnsupportedOperationException(
					"all numbers are supposed to be parsed as long numbers and subsequently converted");
		}
	}

	protected static void stringToText(String str, Set<Character> whatToQuote,
			StringBuffer result) {
		for (int i = 0; i < str.length(); ++i) {
			char ch = str.charAt(i);
			if (whatToQuote.contains(ch))
				result.append('\\');
			result.append(ch);
		}
	}

	public static final int erlMapBegin = 1;
	public static final int erlTupleBegin = 2;
	public static final int erlTupleEnd = 3;
	public static final int erlListBegin = 4;
	public static final int erlListEnd = 5;
	public static final int erlMapSep =  6;
	public static final int erlBitStrBegin = 7;
	public static final int erlBitStrEnd = 8;
	public static final int erlAtomQuote = 9;
	public static final int erlString = 10;
	public static final int erlPositiveNumber = 11;
	public static final int erlNegativeNumber = 12;
	public static final int erlBackslash = 13;
	public static final int erlComma = 14;
	public static final int erlBar = 15;
	public static final int erlSpaces = 16;
	public static final int erlGT = 17;
	public static final int erlLT = 18;
	public static final int erlCol = 19;
	public static final int erlMinus = 20;
	public static final int erlPlus = 21;
	public static final int erlSlash = 22;
	public static final int erlDot = 23;
	public static final int erlE = 24;
	public static final int erlText = 25;

	public static Lexer buildLexer(String whatToParse) {
		return new Lexer(
				"(\\s*#\\{\\s*)|" + // erlMapBegin
				"(\\s*\\{\\s*)|" + // erlTupleBegin
				"(\\s*}\\s*)|" + // erlTupleEnd
				"(\\s*\\[\\s*)|" + // erlListBegin
				"(\\s*]\\s*)|" + // erlListEnd
				"(\\s*=>\\s*)|" + // erlMapSep
				"(\\s*<<\\s*)|" + // erlBitStrBegin
				"(\\s*>>\\s*)|" + // erlBitStrEnd
				"(')|" + // erlAtomQuote
				"(\")|" + // erlString
				"(\\s*\\+?\\d+\\s*)|" + // erlPositiveNumber
				"(\\s*-\\d+\\s*)|" + // erlNegativeNumber
				"(\\\\)|" + // erlBackslash
				"(\\s*,\\s*)|" + // erlComma
				"(\\s*\\|\\s*)|" + // erlBar
				"(\\s+)|" + // erlSpaces
				"(>)|" + // erlGT
				"(<)|" + // erlLT
				"(:)|" + // erlCol
				"(-)|" + // erlMinus
				"(\\+)|" + // erlMinus
				"(/)|" + // erlSlash
				"(\\.)|" + // erlDot
				"([eE])|" + // erlE
				"([^\\\\\"',\\|\\[\\]{}<>:\\-/ ]+)" // erlText, together with
												// spaces but none of the
												// special characters above.
		, whatToParse);

	}

	/**
	 * Given a lexer containing the first term of the expression to parse,
	 * parses the text and returns the corresponding Erlang term.
	 * 
	 * @param lexer lexer set up to go through tokens of the label to parse
	 * @return the outcome.
	 */
	public static OtpErlangObject parseFirstTermInText(Lexer lexer) {
		int currentMatch = lexer.getMatchType();
		while (currentMatch == erlSpaces) {// parsing ErlangTuple
			currentMatch = lexer.getMatchType();
		}
		if (currentMatch < 0)
			throw new IllegalArgumentException("empty term");

		OtpErlangObject result = null;
		switch (currentMatch) {
		case erlMapBegin:
		case erlTupleBegin:
		case erlListBegin:
		case erlBitStrBegin:
		case erlAtomQuote:
		case erlString:
		case erlE:
		case erlDot:
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
			throw new IllegalArgumentException("invalid token type "
					+ currentMatch + " in parsing erlang term, looking at "
					+ lexer.getMatch());
		}
		while (currentMatch == erlSpaces) {// parsing ErlangTuple

			currentMatch = lexer.getMatchType();
		}
		assert result != null;
		return result;
	}

	/**
	 * Given a string containing the whole of the expression to parse, parses
	 * the text and returns the corresponding Erlang term.
	 * 
	 * @param str
	 *            text to parse
	 * @return the outcome.
	 */
	public static OtpErlangObject parseText(String str) {
		Lexer lexer = buildLexer(str);
		OtpErlangObject result = parseFirstTermInText(lexer);
		if (lexer.getLastMatchType() >= 0)// did not get to the end of string
			throw new IllegalArgumentException(
					"unexpected characters at the end of string to parse, looking at "
							+ lexer.remaining());
		return result;
	}

	protected final static Map<Class<? extends OtpErlangObject>, ErlangParserComponent> classToDumper;
	static {
		classToDumper = new HashMap<Class<? extends OtpErlangObject>, ErlangParserComponent>(){

			/**
			 * ID for serialization
			 */
			private static final long serialVersionUID = 1246967667685308150L;
			
			@Override
			public ErlangParserComponent get(Object cls)
			{
				ErlangParserComponent result = super.get(cls);
				if (result == null)
					throw new IllegalArgumentException("dumping of class "+cls+" is not supported");
				return result;
			}
		};
		classToDumper.put(OtpErlangMap.class, ErlangMap.getSingleton());
		classToDumper.put(OtpErlangTuple.class, ErlangTuple.getSingleton());
		classToDumper.put(OtpErlangList.class, ErlangList.getSingleton());
		classToDumper.put(OtpErlangAtom.class, ErlangQuotedAtom.getSingleton());
		classToDumper.put(OtpErlangBoolean.class, ErlangBoolean.getSingleton());
		classToDumper.put(OtpErlangString.class, ErlangString.getSingleton());
		classToDumper.put(OtpErlangLong.class, ErlangLong.getSingleton());
		classToDumper.put(OtpErlangInt.class, ErlangInt.getSingleton());
		
		// This one is a derivative of OtpErlangBitstr.
		classToDumper
			.put(OtpErlangBinary.class, ErlangBitString.getSingleton());
		classToDumper
			.put(OtpErlangBitstr.class, ErlangBitString.getSingleton());
		classToDumper.put(OtpErlangFloat.class, ErlangDouble.getSingleton());
		classToDumper.put(OtpErlangDouble.class, ErlangDouble.getSingleton());
		classToDumper.put(OtpErlangChar.class, ErlangChar.getSingleton());
		classToDumper.put(OtpErlangByte.class, ErlangByte.getSingleton());
	}
	protected final static Map<Integer, ErlangParserComponent> tokenToParser;
	static {
		tokenToParser = new TreeMap<Integer, ErlangParserComponent>();

		tokenToParser.put(erlMapBegin, ErlangMap.getSingleton());
		tokenToParser.put(erlTupleBegin, ErlangTuple.getSingleton());
		tokenToParser.put(erlListBegin, ErlangList.getSingleton());
		tokenToParser.put(erlAtomQuote, ErlangQuotedAtom.getSingleton());
		tokenToParser.put(erlString, ErlangString.getSingleton());
		tokenToParser.put(erlPositiveNumber, ErlangLong.getSingleton());
		tokenToParser.put(erlNegativeNumber, ErlangLong.getSingleton());
		tokenToParser.put(erlE, ErlangUnquotedAtom.getSingleton());
		tokenToParser.put(erlDot, ErlangUnquotedAtom.getSingleton());
		tokenToParser.put(erlText, ErlangUnquotedAtom.getSingleton());
		tokenToParser.put(erlBitStrBegin, ErlangBitString.getSingleton());
	}

	/**
	 * Builds a semblance of Erlang label from a tuple. This is not a complete
	 * label - it cannot really be passed to Erlang for execution because the
	 * function it corresponds to is not defined.
	 */
	public static ErlangLabel erlangObjectToLabel(OtpErlangObject obj, Configuration config) 
	{
		if (!(obj instanceof OtpErlangTuple))
			throw new IllegalArgumentException("expected a tuple, parsed "+ obj);
		OtpErlangTuple tuple = (OtpErlangTuple) obj;

		ErlangLabel outcome = null;
		switch (tuple.arity()) {
		case 2:
			if (!(tuple.elementAt(0) instanceof OtpErlangAtom))
				throw new IllegalArgumentException(
						"function name should be an atom, got "
								+ tuple.elementAt(0));
			outcome = new ErlangLabel(null,
					((OtpErlangAtom) tuple.elementAt(0)).atomValue(),
					tuple.elementAt(1), null);
			break;
		case 3:
			if (!(tuple.elementAt(0) instanceof OtpErlangAtom))
				throw new IllegalArgumentException(
						"first element of a tuple should be an atom, got "
								+ tuple.elementAt(0));
			if (((OtpErlangAtom) tuple.elementAt(0)).atomValue().equals(
					missingFunction)) {
				OtpErlangObject funcName = tuple.elementAt(1);
				if (!(funcName instanceof OtpErlangAtom))
					throw new IllegalArgumentException(
							"function name should be an atom, got " + funcName);
				outcome = new ErlangLabel(null,
						((OtpErlangAtom) funcName).atomValue(),
						tuple.elementAt(2), null);
			} else
				outcome = new ErlangLabel(null,
						((OtpErlangAtom) tuple.elementAt(0)).atomValue(),
						tuple.elementAt(1), tuple.elementAt(2));
			break;
		case 4:
			if (!(tuple.elementAt(0) instanceof OtpErlangAtom)
					|| !((OtpErlangAtom) tuple.elementAt(0)).atomValue()
							.equals(missingFunction))
				throw new IllegalArgumentException(
						"tuples of length 4 should start with "
								+ missingFunction + ", got "
								+ tuple.elementAt(0));
			if (!(tuple.elementAt(1) instanceof OtpErlangAtom))
				throw new IllegalArgumentException(
						"function name should be an atom, got "
								+ tuple.elementAt(0));
			outcome = new ErlangLabel(null,
					((OtpErlangAtom) tuple.elementAt(1)).atomValue(),
					tuple.elementAt(2), tuple.elementAt(3));
			break;
		default:
			throw new IllegalArgumentException(
					"expected Erlang label, got tuple of arity "
							+ tuple.arity() + " in " + tuple);
		}

		// now assign a function name
		ErlangModule mod = ErlangModule
				.findModule(config.getErlangModuleName());
		if (mod != null)
			outcome = mod.behaviour.convertErlToMod(outcome);
		return outcome;
	}

	@Override
	public int toInt() {
		throw new UnsupportedOperationException("Erlang label does not have support for toInt");
	}

}
