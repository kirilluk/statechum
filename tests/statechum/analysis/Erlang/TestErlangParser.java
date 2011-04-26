package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import org.junit.Assert;
import org.junit.Test;

import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import com.ericsson.otp.erlang.OtpErlangAtom;
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
	}
	
	/** Junk at the end of term. */
	@Test
	public void testParse0b()
	{
		final String text = "\'this is an atom\' junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected characters at the end of string to parse");
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
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text,ErlangLabel.dumpErlangObject(atom));
	}
	
	/** With spaces. */
	@Test
	public void testParse1b()
	{
		String text = "   \'this is an atom\'";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1c()
	{
		String text = " \'this \\'is an\\\\  \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is an\\  \"atom",atom.atomValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1d()
	{
		String text = " \'this \\'is \\\"fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is \"fh an \"atom",atom.atomValue());
		Assert.assertEquals("\'this \\'is \"fh an \"atom\'",ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1e()
	{
		String text = " \'this \\' is \\\" fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this ' is \" fh an \"atom",atom.atomValue());
		Assert.assertEquals("\'this \\' is \" fh an \"atom\'",ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1f()
	{
		String text = " \'this\\'i s\\\" fh an \"atom\'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this'i s\" fh an \"atom",atom.atomValue());
		Assert.assertEquals("\'this\\'i s\" fh an \"atom\'",ErlangLabel.dumpErlangObject(atom));
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
				"567some-,text",
				
		})
		{
			final String data = ""+startStop+str+startStop;
			OtpErlangObject smth = ErlangLabel.parseText(data);
			// verify that the string can be parsed ok.
			Assert.assertEquals("parse of "+data+" gave wrong value",data,ErlangLabel.dumpErlangObject(smth));
		}
		for(String str:new String[]{
				"\\-text",
				"\\  -text",
				"\\text",
				"\\56",
				"\\,"
		})
		{
			final String data = str;
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(""+startStop+data+startStop);
			}},IllegalArgumentException.class,"is not supposed to be prefixed");
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
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(text,ErlangLabel.dumpErlangObject(string));
	}
	
	/** With spaces. */
	@Test
	public void testParse2b()
	{
		String text = "   \"this is a \\\\ string\"";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(string));
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
		Assert.assertEquals("\"this \\\"is 'fh an \'atom\"",ErlangLabel.dumpErlangObject(string));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2e()
	{
		String text = " \"this \\\" is \\' fh an \'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \" is ' fh an 'atom",string.stringValue());
		Assert.assertEquals("\"this \\\" is ' fh an 'atom\"",ErlangLabel.dumpErlangObject(string));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2f()
	{
		String text = " \"this\\\"i s\\' fh an \'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this\"i s' fh an \'atom",string.stringValue());
		Assert.assertEquals("\"this\\\"i s' fh an \'atom\"",ErlangLabel.dumpErlangObject(string));
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
	
	
	@Test
	public void testParse3()
	{
		String text = " -45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(-45,number.longValue());
		
	}
	
	@Test
	public void testParse4()
	{
		String text = " 45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(45,number.longValue());
		
	}
	
	@Test
	public void testParse4fail1()
	{
		final String text = " 45junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"in parsing erlang number");
	}
	
	@Test
	public void testParse4fail2()
	{
		final String text = " 45 junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"in parsing erlang number");
	}
	
	@Test
	public void testParse5()
	{
		String text = " 4588888888888888 ";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(4588888888888888L,number.longValue());
		
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
	}
	/** Invalid characters after backslash. */
	@Test
	public void testParseUnquotedAtomFail2()
	{
		final String text = "thi\\\"s ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"is never allowed in an atom");
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
	public void testParseTuple1()
	{
		for(String text:new String[]{
				" { a, b}"," { a, b }"," {a, b}"," {a,b}"})
		{
			OtpErlangTuple tuple = (OtpErlangTuple)ErlangLabel.parseText(text);
			Assert.assertEquals("{'a','b'}",ErlangLabel.dumpErlangObject(tuple));
		}
	}
	
	@Test
	public void testParseTuple2()
	{
		for(String text:new String[]{
				" { }"," {}","{}"," { } "})
		{
			OtpErlangTuple tuple = (OtpErlangTuple)ErlangLabel.parseText(text);
			Assert.assertEquals("{}",ErlangLabel.dumpErlangObject(tuple));
		}
	}
	
	@Test
	public void testParseTuple3()
	{
		for(String text:new String[]{
				" { a }"," {a}","{a }"," {  a} "})
		{
			OtpErlangTuple tuple = (OtpErlangTuple)ErlangLabel.parseText(text);
			Assert.assertEquals("{'a'}",ErlangLabel.dumpErlangObject(tuple));
		}
	}
	
	@Test
	public void testParseTuple1Fail1()
	{
		final String text = "{a";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
	}
	
	@Test
	public void testParseTuple1Fail2()
	{
		final String text = "{a{";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseTuple1Fail3()
	{
		final String text = "{a{}";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseTuple1Fail4()
	{
		final String text = "{a,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
	}
	
	@Test
	public void testParseTuple1Fail5()
	{
		final String text = "{a{ ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseTuple1Fail6()
	{
		final String text = "{a , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
	}
	
	@Test
	public void testParseTuple1Fail7()
	{
		final String text = "{,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing tuple");
	}
	
	@Test
	public void testParseTuple1Fail8()
	{
		final String text = "{ , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing tuple");
	}
	
	@Test
	public void testParseTuple1Fail9()
	{
		final String text = "{ 56, hh";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of tuple");
	}
	
	@Test
	public void testParseList1()
	{
		for(String text:new String[]{
				" [ a, b]"," [ a, b ]"," [a, b]"," [a,b]"})
		{
			OtpErlangList list = (OtpErlangList)ErlangLabel.parseText(text);
			Assert.assertEquals("['a','b']",ErlangLabel.dumpErlangObject(list));
		}
	}
	
	@Test
	public void testParseList2()
	{
		for(String text:new String[]{
				" [ ]"," []","[]"," [ ] "})
		{
			OtpErlangList list = (OtpErlangList)ErlangLabel.parseText(text);
			Assert.assertEquals("[]",ErlangLabel.dumpErlangObject(list));
		}
	}
	
	@Test
	public void testParseList3()
	{
		for(String text:new String[]{
				" [ a ]"," [a]","[a ]"," [  a] "})
		{
			OtpErlangList list = (OtpErlangList)ErlangLabel.parseText(text);
			Assert.assertEquals("['a']",ErlangLabel.dumpErlangObject(list));
		}
	}

	@Test
	public void testParseList1Fail1()
	{
		final String text = "[a";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
	}
	
	@Test
	public void testParseList1Fail2()
	{
		final String text = "[a[";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseList1Fail3()
	{
		final String text = "[a{}";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseList1Fail4()
	{
		final String text = "[a,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
	}
	
	@Test
	public void testParseList1Fail5()
	{
		final String text = "[a{ ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"expecting comma");
	}
	
	@Test
	public void testParseList1Fail6()
	{
		final String text = "[a , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
	}
	
	@Test
	public void testParseList1Fail7()
	{
		final String text = "[,";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing list");
	}
	
	@Test
	public void testParseList1Fail8()
	{
		final String text = "[ , ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected comma in parsing list");
	}
	
	@Test
	public void testParseList1Fail9()
	{
		final String text = "[ 56, hh";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"unexpected end of list");
	}
	
	@Test
	public void testParseBig()
	{
		final String text = "{\'this is an atom\',\"this is a string\",[[-234],{}]}";
		OtpErlangObject obtained = ErlangLabel.parseText(text);
		Assert.assertEquals(text, ErlangLabel.dumpErlangObject(obtained));
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

}
