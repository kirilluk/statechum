package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.math.BigInteger;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import cern.colt.Arrays;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class TestErlangParser {

	@Test
	public void testToString1()
	{
		Assert.assertEquals("\'this is an atom\'",ErlangLabel.dumpErlangObject(new OtpErlangAtom("this is an atom")));
	}
	
	@Test
	public void testToString2()
	{
		Assert.assertEquals("\"this is a string\"",ErlangLabel.dumpErlangObject(new OtpErlangString("this is a string")));
	}
	
	@Test
	public void testToString3()
	{
		Assert.assertEquals("[\'this is an atom\',\"this is a string\"]",
				ErlangLabel.dumpErlangObject(new OtpErlangList(new OtpErlangObject[]{
						new OtpErlangAtom("this is an atom"),
						new OtpErlangString("this is a string")
				})));
	}
	
	@Test
	public void testToString4()
	{
		Assert.assertEquals("{\'this is an atom\',\"this is a string\"}",
				ErlangLabel.dumpErlangObject(new OtpErlangTuple(new OtpErlangObject[]{
						new OtpErlangAtom("this is an atom"),
						new OtpErlangString("this is a string")
				})));
	}
	
	@Test
	public void testToString5()
	{
		Assert.assertEquals("[]",
				ErlangLabel.dumpErlangObject(new OtpErlangList(new OtpErlangObject[]{
				})));
	}
	
	@Test
	public void testToString6()
	{
		Assert.assertEquals("{}",
				ErlangLabel.dumpErlangObject(new OtpErlangTuple(new OtpErlangObject[]{
				})));
	}
	
	@Test
	public void testToString7a()
	{
		Assert.assertEquals("-234",ErlangLabel.dumpErlangObject(new OtpErlangLong(-234)));
	}
	
	@Test
	public void testToString7b()
	{
		Assert.assertEquals("234",ErlangLabel.dumpErlangObject(new OtpErlangLong(234)));
	}
	
	@Test
	public void testToString8()
	{
		Assert.assertEquals("-234",ErlangLabel.dumpErlangObject(new OtpErlangInt(-234)));
	}
	
	@Test
	public void testToString9()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.dumpErlangObject(new OtpErlangFloat(-234));
		}},IllegalArgumentException.class,"cannot dump object of type");
	}
	
	
	/** A bigger structure to dump. */
	@Test
	public void testToString10()
	{
		Assert.assertEquals("{\'this is an atom\',\"this is a string\",[[-234],{}]}",
				ErlangLabel.dumpErlangObject(new OtpErlangTuple(new OtpErlangObject[]{
						new OtpErlangAtom("this is an atom"),
						new OtpErlangString("this is a string"),
						new OtpErlangList(new OtpErlangObject[]{
								new OtpErlangList(new OtpErlangObject[]{
										new OtpErlangInt(-234)
								}),
								new OtpErlangTuple(new OtpErlangObject[]{
								})
						})
				})));
	}
	
	/** Empty term. */
	@Test
	public void testParse0a()
	{
		final String text = "  ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"empty term");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	/** Junk at the end of term. */
	@Test
	public void testParse0b()
	{
		final String text = "\'this is an atom\' junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected characters at the end of string to parse");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	/** Junk at the end of term which can be retrieved. */
	@Test
	public void testParse0c()
	{
		final String text = "\'this is an atom\' junk and more";
    	Lexer lexer = ErlangLabel.buildLexer(text);
    	OtpErlangObject result = ErlangLabel.parseFirstTermInText(lexer);
		OtpErlangAtom atom = (OtpErlangAtom)result;
		Assert.assertEquals("\'this is an atom\'",ErlangLabel.dumpErlangObject(atom));
		Assert.assertEquals("junk and more",lexer.remaining());
	}
	
	@Test
	public void testParse1a()
	{
		String text = "\'this is an atom\'";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(text,text);
	}
	
	/** With spaces. */
	@Test
	public void testParse1b()
	{
		String text = "   \'this is an atom\'";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(text.trim(),text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1c()
	{
		String text = " \'this \\'is an\\\\  \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is an\\  \"atom",atom.atomValue());
		checkResponse(text.trim(),ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1d()
	{
		String text = " \'this \\'is \\\"fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is \"fh an \"atom",atom.atomValue());
		checkResponse("\'this \\'is \"fh an \"atom\'",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1e()
	{
		String text = " \'this \\' is \\\" fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this ' is \" fh an \"atom",atom.atomValue());
		checkResponse("\'this \\' is \" fh an \"atom\'",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1f()
	{
		String text = " \'this\\'i s\\\" fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this'i s\" fh an \"atom",atom.atomValue());
		checkResponse("\'this\\'i s\" fh an \"atom\'",text);
	}
	
	/** Invalid characters after backslash. */
	public static void testParseChokesOnInvalidChars(final char startStop)
	{
		for(String str:new String[]{
				"valid text",
				"some-text",
				"some -text",
				"some- text",
				"some - , text",
				"some-,text",
				"567some-,text"
		})
		{
			final String data = ""+startStop+str+startStop;
			//OtpErlangObject smth = ErlangLabel.parseText(data);
			// verify that the string can be parsed ok.
			//Assert.assertEquals("parse of "+data+" gave wrong value",data,ErlangLabel.dumpErlangObject(smth));
			checkResponse(data,data);
		}
		
		for(String str:new String[]{
				"\\-text",
				"\\  -text",
				"\\:text",
				"\\  :text",
				"\\>text",
				"\\<  :text",
				"\\,"
		})
		{
			final String data = ""+startStop+str+startStop;
			//OtpErlangObject smth = ErlangLabel.parseText(data);
			// verify that the string can be parsed ok.
			//Assert.assertEquals("parse of "+data+" gave wrong value",data,ErlangLabel.dumpErlangObject(smth));
			checkResponse(data.replace(""+startStop+"\\",""+startStop),data);
		}
		for(String str:new String[]{
				"\\text",// this notation is not supported at the moment
				"\\56" // this notation is not supported at the moment
		})
		{
			final String data = str;
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(""+startStop+data+startStop);
			}},IllegalArgumentException.class,"is not supposed to be prefixed");
			
			// We are a lot more restrictive here compared to the actual Erlang runtime - all of the above 
			// ges through fine.
			ErlangRunner.getRunner().evaluateString(""+startStop+data+startStop);
		}
		
	}
	
	/** Invalid characters after backslash. */
	@Test
	public void testParse1Fail1()
	{
		final String text = "\'this is an atom\\\" ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of atom");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
		
	@Test
	public void testParse1Fail2()
	{
		testParseChokesOnInvalidChars('\'');
	}
	
	@Test
	public void testParses2a()
	{
		String text = "\"this is a string\"";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangString);
		checkResponse(text,text);
	}
	
	/** With spaces. */
	@Test
	public void testParse2b()
	{
		String text = "   \"this is a \\\\ string\"";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangString);
		checkResponse(text.trim(),text);
	}
	
	public void testParse2c()
	{
		String text = " \"this \\\"is an 'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \"is an 'atom",string.stringValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(string));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2d()
	{
		String text = " \"this \\\"is \\'fh an \'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \"is 'fh an 'atom",string.stringValue());
		checkResponse("\"this \\\"is 'fh an \'atom\"",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2e()
	{
		String text = " \"this \\\" is \\' fh an \'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \" is ' fh an 'atom",string.stringValue());
		checkResponse("\"this \\\" is ' fh an 'atom\"",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2f()
	{
		String text = " \"this\\\"i s\\' fh an \'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this\"i s' fh an \'atom",string.stringValue());
		checkResponse("\"this\\\"i s' fh an \'atom\"",text);
	}
	

	/** Invalid characters after backslash. */
	@Test
	public void testParse2Fail1()
	{
		final String text = "\"this is an atom\\\" ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of string");
	}
		
	@Test
	public void testParse2Fail2()
	{
		testParseChokesOnInvalidChars('\"');
	}
	
	/** Checks that both our parser and Erlang runtime produce the same response to parsing the supplied chunk of text. */
	protected static void checkResponse(String expectedOutcome, String text)
	{
		Assert.assertEquals(expectedOutcome,ErlangLabel.dumpErlangObject(ErlangLabel.parseText(text)));
		Assert.assertEquals(expectedOutcome,ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse3()
	{
		String text = " -45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(-45,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse4()
	{
		String text = " 45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(45,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse4fail1()
	{
		final String text = " 45junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"in parsing erlang number");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParse4fail2()
	{
		final String text = " 45 junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"in parsing erlang number");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParse5()
	{
		String text = " 4588888888888888 ";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(4588888888888888L,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParseUnquotedAtom1()
	{
		String text = "atom";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	@Test
	public void testParseUnquotedAtom2()
	{
		String text = " atom ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	@Test
	public void testParseUnquotedAtom3()
	{
		String text = "atom ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	
	/** Invalid characters after backslash. */
	@Test
	public void testParseUnquotedAtomFail1()
	{
		final String text = "thi\\s ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"is never allowed in an atom");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}

	/** Invalid characters after backslash. */
	@Test
	public void testParseUnquotedAtomFail2()
	{
		final String text = "thi\\\"s ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"is never allowed in an atom");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseUnquotedAtomFail3()
	{
		final String text = " aa junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected characters at the end of string to parse");
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseFirstTermInText(ErlangLabel.buildLexer(text));
		Assert.assertEquals("aa",atom.atomValue());
	}
	
	@Test
	public void testParseUnquotedAtomFail4()
	{
		for(String str:new String[]{
		"a-atom",
		"a:atom",
		"atom-a",
		"atom:b"})
		{
			final String text = str;
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,"is never allowed");
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangRunner.getRunner().evaluateString(text);
			}},RuntimeException.class,"evaluation");
			
		}
	}
	
	@Test
	public void testParseUnquotedAtomFail5()
	{
		for(String str:new String[]{
		"a>atom",
		"a<atom"})
		{
			final String text = str;
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,"is never allowed");
			// Erlang has a different opinion on this, interpreting it as an expression.
		}
	}
	
	@Test
	public void testParseJunkInTheBeginning()
	{
		for(String str:new String[]{
		"-atom",
		":atom",
		">atom",
		"<atom"})
		{
			final String text = str;
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,"invalid token");
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangRunner.getRunner().evaluateString(text);
			}},RuntimeException.class,"evaluation");
			
		}
	}
	
	@Test
	public void testParseTuple1()
	{
		for(String text:new String[]{
				" { a, b}"," { a, b }"," {a, b}"," {a,b}"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse("{'a','b'}",text);
		}
	}
	
	@Test
	public void testParseTuple2()
	{
		for(String text:new String[]{
				" { }"," {}","{}"," { } "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse("{}",text);
		}
	}
	
	@Test
	public void testParseTuple3()
	{
		for(String text:new String[]{
				" { a }"," {a}","{a }"," {  a} "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse("{'a'}",text);
		}
	}
	
	@Test
	public void testParseTuple1Fail1()
	{
		final String text = "{a";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail2()
	{
		final String text = "{a{";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail3()
	{
		final String text = "{a{}";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail4()
	{
		final String text = "{a,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail5()
	{
		final String text = "{a{ ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail6()
	{
		final String text = "{a , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail7()
	{
		final String text = "{,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail8()
	{
		final String text = "{ , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail9()
	{
		final String text = "{ 56, hh";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1()
	{
		for(String text:new String[]{
				" [ a, b]"," [ a, b ]"," [a, b]"," [a,b]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse("['a','b']",text);
		}
	}
	
	@Test
	public void testParseList2()
	{
		for(String text:new String[]{
				" [ ]"," []","[]"," [ ] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse("[]",text);
		}
	}
	
	@Test
	public void testParseList3()
	{
		for(String text:new String[]{
				" [ a ]"," [a]","[a ]"," [  a] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse("['a']",text);
		}
	}

	@Test
	public void testParseList1Fail1()
	{
		final String text = "[a";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail2()
	{
		final String text = "[a[";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail3()
	{
		final String text = "[a{}";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail4()
	{
		final String text = "[a,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail5()
	{
		final String text = "[a{ ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail6()
	{
		final String text = "[a , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail7()
	{
		final String text = "[,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail8()
	{
		final String text = "[ , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail9()
	{
		final String text = "[ 56, hh";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testParseBig()
	{
		final String text = "{\'this is an atom\',\"this is a string\",[[-234],{}]}";
		OtpErlangObject obtained = ErlangLabel.parseText(text);
		checkResponse(text, text);
		Assert.assertEquals(new OtpErlangTuple(new OtpErlangObject[]{
						new OtpErlangAtom("this is an atom"),
						new OtpErlangString("this is a string"),
						new OtpErlangList(new OtpErlangObject[]{
								new OtpErlangList(new OtpErlangObject[]{
										new OtpErlangInt(-234)
								}),
								new OtpErlangTuple(new OtpErlangObject[]{
								})
						})
				}),obtained);
	}

	@Test
	public void testParseBoolean()
	{
		for(String text:new String[]{
				" true "," false "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangBoolean);
			Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangLabel.parseText(text)));
			// Erlang always returns atoms - cannot really blame it for that.
		}
	}

	@Test
	public void testDumpBitStr0()
	{
		OtpErlangBitstr str = (OtpErlangBitstr) ErlangRunner.getRunner().evaluateString("<<  >>");
		Assert.assertEquals("<< >>",ErlangLabel.dumpErlangObject(str));
	}
	
	@Test
	public void testDumpBitStr1()
	{
		OtpErlangBitstr str = (OtpErlangBitstr) ErlangRunner.getRunner().evaluateString("<< 1:4,0:4/integer-unit:1,1:1,0:7 >>");
		Assert.assertEquals("<< 16, 128>>",ErlangLabel.dumpErlangObject(str));
	}
	
	@Test
	public void testDumpBitStr2()
	{
		OtpErlangBitstr str = new OtpErlangBitstr(new byte[]{-128,8},0);
		Assert.assertEquals("<< 128, 8>>",ErlangLabel.dumpErlangObject(str));
	}
	
	@Test
	public void testDumpBitStrFail()
	{
		final OtpErlangBitstr str = (OtpErlangBitstr) ErlangRunner.getRunner().evaluateString("<< 1:4,0:4/integer-unit:1,1:1 >>");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.dumpErlangObject(str);
		}},IllegalArgumentException.class,"is not divisible by 8");
	}
	
	
	@RunWith(Parameterized.class)
	public static class TestParseBitStr
	{
		@Parameters
		public static Collection<Object[]> data() 
		{
			final String badmatch = "badmatch"; // it is always so for real Erlang because we'll get an error which correspond to a pattern match failure. 
			return java.util.Arrays.asList(new Object[][]{
					new Object[]{"<<","unexpected end of",badmatch},
					new Object[]{"<< <","invalid token",badmatch},
					new Object[]{"<< <<","invalid token",badmatch},
					new Object[]{"<< >","invalid token",badmatch},
					new Object[]{"<< ,","got , in NUM_OR_END",badmatch},
					new Object[]{"<< :","got : in NUM_OR_END",badmatch},
					new Object[]{"<< a","type is unexpected",badmatch},
					new Object[]{"<< -","dash can only be used after a type",badmatch},
					new Object[]{"<< /","slash can only be used after NUMBER:SIZE",badmatch},
					
					new Object[]{"<< -56345345345","unexpected end",badmatch},
					new Object[]{"<< -56,","unexpected end",badmatch},
					new Object[]{"<< -56, >","invalid token",badmatch},
					new Object[]{"<< -56 -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56>","invalid token",badmatch},
					new Object[]{"<< -56<","invalid token",badmatch},
					new Object[]{"<< -56<<","invalid token",badmatch},
					new Object[]{"<< -56/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:","unexpected end",badmatch},

					new Object[]{"<< -56:345345345","unexpected end",badmatch},
					new Object[]{"<< -56:,","got , in SIZE",badmatch},
					new Object[]{"<< -56: >","invalid token",badmatch},
					new Object[]{"<< -56: -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:>","invalid token",badmatch},
					new Object[]{"<< -56:<","invalid token",badmatch},
					new Object[]{"<< -56:<<","invalid token",badmatch},
					new Object[]{"<< -56:/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56::","got : in SIZE",badmatch},

					new Object[]{"<< -56:34/,","got , in TYPE",badmatch},
					new Object[]{"<< -56:34/ >","invalid token",badmatch},
					new Object[]{"<< -56:34/ -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/>","invalid token",badmatch},
					new Object[]{"<< -56:34/<","invalid token",badmatch},
					new Object[]{"<< -56:34/<<","invalid token",badmatch},
					new Object[]{"<< -56:34//","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/:","got : in TYPE",badmatch},
			
					new Object[]{"<< -56:34/unit","unexpected end",badmatch},
					new Object[]{"<< -56:34,","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34 >","invalid token",badmatch},
					new Object[]{"<< -56:34 -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34>","invalid token",badmatch},
					new Object[]{"<< -56:34<","invalid token",badmatch},
					new Object[]{"<< -56:34<<","invalid token",badmatch},
					new Object[]{"<< -56:34/","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34:","got : in SLASH_COMMA_END",badmatch},

					new Object[]{"<< -56:34/signed","unexpected end",badmatch},
					new Object[]{"<< -56:34/integer","unexpected end",badmatch},
					new Object[]{"<< -56:34/signed-signed-signed","unexpected end",badmatch},
					new Object[]{"<< -56:34/unsigned-signed","already unsigned",badmatch},
					new Object[]{"<< -56:34/signed-unsigned","already signed",badmatch},
					new Object[]{"<< -56:34/big-signed","unexpected end",badmatch},
					new Object[]{"<< -56:34/big-little","already big",badmatch},
					new Object[]{"<< -56:34/little-big","already little",badmatch},
					new Object[]{"<< -56:34/unknown","unknown type specifier",badmatch},
					new Object[]{"<< -56:34/55","got 55 in TYPE",badmatch},
					new Object[]{"<< -56:34/unit","unexpected end",badmatch},
					
					new Object[]{"<< -56:34/unit,","got , in UCOLON",badmatch},
					new Object[]{"<< -56:34/unit >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:","unexpected end of bit string",badmatch},

					new Object[]{"<< -56:34/signed,","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed >","invalid token",badmatch},
					new Object[]{"<< -56:34/signed -","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed-","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed>","invalid token",badmatch},
					new Object[]{"<< -56:34/signed<","invalid token",badmatch},
					new Object[]{"<< -56:34/signed<<","invalid token",badmatch},
					new Object[]{"<< -56:34/signed/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/signed:","got : in MINUS_COMMA",badmatch},

					new Object[]{"<< -56:34/unit:,","got , in UNIT",badmatch},
					new Object[]{"<< -56:34/unit: >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit: -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit::","got : in UNIT",badmatch},

					new Object[]{"<< -56:34/unit:5,","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/unit:5 >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5 -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:5:","got : in COMMA_END",badmatch},

					new Object[]{"<< -56:34/unit:5,,","got , in NUM",badmatch},
					new Object[]{"<< -56:34/unit:5, >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5, -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5,-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5,>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:5,:","got : in NUM",badmatch},
			});
		}
		
		final String text,exception, erlEx; 
		
		/** Creates the test class with the number of threads to create as an argument. */
		public TestParseBitStr(String textArg,String exceptionArg, String erlExArg)
		{
			text = textArg;exception = exceptionArg;erlEx = erlExArg;
		}
		
		public static String parametersToString(String textArg,String exceptionArg, @SuppressWarnings("unused") String erlExArg)
		{
			return textArg+" - "+exceptionArg;
		}
		
		@Test
		public void testBitStrFail1()
		{
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,exception);
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangRunner.getRunner().evaluateString(text);
			}},RuntimeException.class,erlEx);
		}
		
	}
	
	// More BigStrParser tests
	
	@Test
	public void testEmpty()
	{
		final String text = "<< >>";
		checkResponse(text, text);
	}
	@Test
	public void testSingleByte()
	{
		final String text = "<< 56 >>";
		checkResponse("<< 56>>", text);
	}
	
	@Test
	public void testTwoBytes()
	{
		final String text = "<<-89,34 >>";
		checkResponse("<< 167, 34>>", text);
	}
	@Test
	public void testPattern1()
	{
		final String text = "<< 1:2,32:6 >>";
		checkResponse("<< 96>>", text);
	}
	
	@Test
	public void testBitStrFail2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText("<< 1:2,32:6,1:1>>");
		}},IllegalArgumentException.class,"should be divisible by 8");
	}
	
	@Test
	public void testPattern2()
	{
		final String text = "<< 1:2,32:6,1:1,0:7 >>";
		checkResponse("<< 96, 128>>", text);
		//System.out.println(Arrays.toString(new BigInteger(""+(256*128+88)).toByteArray()));
	}
	
	@Test
	public void testPattern3a()
	{
		final String text = "<< 1:2,1:2,0:4 >>";
		checkResponse("<< 80>>", text);
	}
	
	@Test
	public void testPattern3b()
	{
		final String text = "<< 1:2/big-unit:2,2:4 >>";
		checkResponse("<< 18>>", text);
	}
	
	@Test
	public void testPattern4a()
	{
		final String text = "<< 1:16 >>";
		checkResponse("<< 0, 1>>", text);
	}
	
	@Test
	public void testPattern4b()
	{
		final String text = "<< 1:15,1:1 >>";
		checkResponse("<< 0, 3>>", text);
	}
	
	@Test
	public void testPattern4c()
	{
		final String text = "<< 1:4,1:11,1:1 >>";
		checkResponse("<< 16, 3>>", text);
	}
	
	@Test
	public void testPattern5a()
	{
		final String text = "<< 1:2,32:6 >>";
		checkResponse("<< 96>>", text);
	}
	
	@Test
	public void testPattern5b()
	{
		final String text = "<< 1:2,32:6,1:1,0:5,0:2 >>";
		checkResponse("<< 96, 128>>", text);
	}
	
	@Test
	public void testPattern5c()
	{
		final String text = "<< 1:1,5:5,0:2 >>";
		checkResponse("<< 148>>", text);
	}
	
	@Test
	public void testPattern5d()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,0:10 >>";
		checkResponse("<< 96, 148, 0>>", text);
	}
	
	@Test
	public void testPattern5e()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:10 >>";
		checkResponse("<< 96, 151, 178>>", text);
	}
	
	@Test
	public void testPattern6a()
	{
		final String text = "<< 1:1,5:5,-78:13,3456454:13 >>";
		checkResponse("<< 151, 246, 93, 198>>", text);
	}
	
	@Test
	public void testPattern6b()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,3456454:13 >>";
		checkResponse("<< 96, 151, 246, 93, 198>>", text);
	}
	
	@Test
	public void testPattern6c()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,567654565765454:30,3456454:13,3:2 >>";
		checkResponse("<< 96, 151, 246, 80, 95, 176, 167, 119, 27>>", text);
	}
	
	@Test
	public void testPattern7a()
	{
		final String text = "<< 2:1/little-unit:16 >>";
		checkResponse("<< 2, 0>>", text);
	}

	@Test
	public void testPattern7b()
	{
		final String text = "<< 567654565765454:32/little >>";
		checkResponse("<< 78, 97, 191, 96>>", text);
	}

	@Test
	public void testPattern7c()
	{
		final String text = "<< 567654565765454:32/little,128 >>";
		checkResponse("<< 78, 97, 191, 96, 128>>", text);
	}
	
	@Test
	public void testPattern7d()
	{
		final String text = "<< 256:9/little,64:7 >>";
		checkResponse("<< 0, 192>>", text);
	}

	@Test
	public void testPattern7e()
	{
		final String text = "<< 256:9/little,-4:31 >>";
		checkResponse("<< 0, 255, 255, 255, 252>>", text);
	}

	@Test
	public void testPattern7g()
	{
		final String text = "<< 1:1,2:15/little>>";
		checkResponse("<< 129, 0>>", text);
	}

	@Test
	public void testPattern7f()
	{
		final String text = "<< 1:1,1:15/little>>";
		checkResponse("<< 128, 128>>", text);
	}

	@Test
	public void testPattern7h()
	{
		final String text = "<< -78:12,567654565765454:28/little >>";
		checkResponse("<< 251, 36, 230, 27, 240>>", text);
	}
	
	@Test
	public void testPattern7i()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,567654565765454:30/little,3456454:13,3:2 >>";
		checkResponse("<< 96, 151, 246, 73, 204, 55, 240, 119, 27>>", text);
	}
	
}
