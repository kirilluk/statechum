package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collection;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import statechum.Configuration;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBitstr;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
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
		Assert.assertEquals("-2.340000138101317E-10",ErlangLabel.dumpErlangObject(new OtpErlangDouble(-2.34e-10f)));
	}
	
	@Test
	public void testToString10()
	{
		Assert.assertEquals("-2.3399999141693115",ErlangLabel.dumpErlangObject(new OtpErlangDouble(-2.34f)));
	}
	
	@Test
	public void testToString11()
	{
		Assert.assertEquals("-2.34E-210",ErlangLabel.dumpErlangObject(new OtpErlangDouble(-2.34e-210)));
	}
	
	
	/** A bigger structure to dump. */
	@Test
	public void testToString12()
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
	
	@Test
	public void testToStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.dumpErlangObject(new OtpErlangRef("aa",1,2));
		}},IllegalArgumentException.class,"cannot dump object of type");
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
	public void testParse1aa()
	{
		String text = "\'\'";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(text,text);
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
	
	/** Spaces. */
	@Test
	public void testParse1g()
	{
		String text = " \'  \'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("  ",atom.atomValue());
		checkResponse("\'  \'",text);
	}	
	
	/** Spaces. */
	@Test
	public void testParse1h()
	{
		String text = " \' a \'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(" a ",atom.atomValue());
		checkResponse("\' a \'",text);
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
	
	/** Spaces. */
	@Test
	public void testParse2g()
	{
		String text = " \"  \"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("  ",string.stringValue());
		checkResponse("\"  \"",text);
	}	
	
	/** Spaces. */
	@Test
	public void testParse2h()
	{
		String text = " \" a \"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(" a ",string.stringValue());
		checkResponse("\" a \"",text);
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
		}},IllegalArgumentException.class,"invalid token type");
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
	public void testParse5a()
	{
		String text = " 45888888888";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(45888888888L,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse5b()
	{
		String text = " -458";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(-458L,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse5c()
	{
		String text = " +458";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(458L,number.longValue());
		Assert.assertEquals("458",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6a()
	{
		String text = " 458888.5";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(458888.5,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("458888.5",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6b()
	{
		String text = " 458888.5e0 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(458888.5,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("458888.5",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6c()
	{
		String text = " 458888.5e-5 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588885,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588885",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6d()
	{
		String text = " 458888.5e5 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588885e10,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588885E10",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6e()
	{
		String text = " 4588.456e34 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588456e37,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588456E37",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6f()
	{
		String text = " 4588888888888888.456e244 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588888888888889E259,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588888888888889E259",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6g()
	{
		String text = " 4588888888888888.456e-244 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588888888888888E-229,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588888888888888E-229",ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	/** When offered the value exactly on the boundary of the lower exponent, the answer given by 
	 * Erlang is completely wrong, hence we do not test this case. 
	 */
	@Test
	public void testParse6ha()
	{
		final String text = " 4.0E"+(-ErlangLabel.ErlangLong.minExponent+1)+" ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4d*Math.pow(10, -ErlangLabel.ErlangLong.minExponent+1),number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6hb()
	{
		final String text = " 4.0e"+(-ErlangLabel.ErlangLong.minExponent-1)+" ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"cannot be represented");
		try
		{
			Assert.assertEquals(0,((OtpErlangDouble)ErlangRunner.getRunner().evaluateString(text)).doubleValue(),Configuration.fpAccuracy);
			// if the exception is not thrown but the assertion is satisfied, this is fine (this is what happens on WinXP).
		}
		catch(Exception ex)
		{// On Linux, an exception is thrown.
			Class<RuntimeException> exceptionClass = RuntimeException.class;
			String exceptionString = "badmatch";
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong type of exception received "+str.toString()+" instead of "+exceptionClass,exceptionClass,ex.getClass());
			Assert.assertTrue("expected exception containing \""+exceptionString+"\" but got \""+ex.getMessage()+"\"",ex.getMessage().contains(exceptionString));
		}
	}
	
	@Test
	public void testParse6ia()
	{
		final String text = " 4.0E"+(ErlangLabel.ErlangLong.maxExponent)+" ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4d*Math.pow(10, ErlangLabel.ErlangLong.maxExponent),number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(ErlangRunner.getRunner().evaluateString(text)));
	}
	
	@Test
	public void testParse6ib()
	{
		final String text = " 4.0e"+(ErlangLabel.ErlangLong.maxExponent+1)+" ";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"cannot be represented");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	@Test
	public void testCheckMaxExponent()
	{
		int i=0;
		for(i=200;i< 400 &&
			!Double.isInfinite(Double.parseDouble("4.0e"+i))
		;++i);
		Assert.assertEquals(ErlangLabel.ErlangLong.maxExponent+1, i);
	}

	@Test
	public void testCheckMinExponent()
	{
		int i=0;
		for(i=-200;i> -400 &&
		0 !=(Double.parseDouble("4.0e"+i))
		;--i);
		Assert.assertEquals(ErlangLabel.ErlangLong.minExponent+1, -i);
	}

	/** Erlang refuses to parse 4e10, I think this is wrong and hence my parser accepts this. */
	@Test
	public void testParse6j()
	{
		final String text = " 4e10 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4e10,number.doubleValue(),Configuration.fpAccuracy);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
	}
	
	/** Erlang refuses to parse 4.e10, I think this is wrong and hence my parser accepts this. */
	@Test
	public void testParse6k()
	{
		final String text = " 4.e10 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4e10,number.doubleValue(),Configuration.fpAccuracy);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"illegal_expr");
	}
	
	@Test
	public void testParseUnquotedAtom0()
	{
		String text = "atom";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	
	@Test
	public void testParseUnquotedAtom1a()
	{
		String text = "eatom";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	
	@Test
	public void testParseUnquotedAtom1b()
	{
		String text = "eateom";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(text.trim(),atom.atomValue());
	}
	
	@Test
	public void testParseUnquotedAtom1c()
	{
		String text = "eat...om";
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
	
	/** Atom cannot begin with a dot. */
	@Test
	public void testParseUnquotedAtomFail6()
	{
		final String text = " .aa junk";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.parseText(text);
		}},IllegalArgumentException.class,"cannot start with a dot");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString(text);
		}},RuntimeException.class,"badmatch");
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
	
	/** invalid atoms. */
	@RunWith(Parameterized.class)
	public static class TestParseInvalidCharsInAtomFail
	{
		@Parameters
		public static Collection<Object[]> data() 
		{
			final String unquotedError = "unquoted atom cannot start with a dot",
			isNeverAllowedError = "is never allowed",invalidTokenError = "invalid token",
			erlEvaluation = "evaluation",erlNone = "";
			return java.util.Arrays.asList(new Object[][]{
					new Object[]{"{ 56, .aa }",unquotedError,erlNone},// Real Erlang accepts this but throws away the dot - I'm not happy about this.
					new Object[]{"{ 56, +aa }",invalidTokenError,erlNone}, 
					new Object[]{"{ 56, -aa }",invalidTokenError,erlNone}, 
					new Object[]{"[ 56, .aa ]",unquotedError,erlNone},
					new Object[]{"[ 56, +aa ]",invalidTokenError,erlNone}, 
					new Object[]{"[ 56, -aa ]",invalidTokenError,erlNone},
					new Object[]{"{ 56, [.aa] }",unquotedError,erlNone},
					new Object[]{"{ 56,[ +aa ]}",invalidTokenError,erlNone}, 
					new Object[]{"{ 56, [-aa ]}",invalidTokenError,erlNone}, 
					new Object[]{"[ 56, {.aa} ]",unquotedError,erlNone},
					new Object[]{"[ 56,{ +aa} ]",invalidTokenError,erlNone}, 
					new Object[]{"[ 56, {-aa }]",invalidTokenError,erlNone},
					new Object[]{"+ [ 56 ]",invalidTokenError,erlNone},
					new Object[]{". [ 56 ]",unquotedError,erlNone},
					new Object[]{"- [ 56 ]",invalidTokenError,erlNone},
					new Object[]{"+[ 56 ]",invalidTokenError,erlNone},
					new Object[]{".[ 56 ]",unquotedError,erlNone},
					new Object[]{"-[ 56 ]",invalidTokenError,erlNone},
					
					new Object[]{"a-atom",isNeverAllowedError,erlNone},
					new Object[]{"a:atom",isNeverAllowedError,erlNone},
					new Object[]{"atom-a",isNeverAllowedError,erlNone},
					new Object[]{"atom:b",isNeverAllowedError,erlNone},
					
					new Object[]{"-atom",invalidTokenError,erlEvaluation},
					new Object[]{"+atom",invalidTokenError,erlEvaluation},
					new Object[]{":atom",invalidTokenError,erlEvaluation},
					new Object[]{">atom",invalidTokenError,erlEvaluation},
					new Object[]{"<atom",invalidTokenError,erlEvaluation},
					
			});
		}
		final String text,exception,erlException; 
					
		public TestParseInvalidCharsInAtomFail(String textArg,String exceptionArg, String erlArg)
		{
			text = textArg;exception = exceptionArg;erlException = erlArg;
		}
		
		public static String parametersToString(String textArg,String exceptionArg, String erlArg)
		{
			return textArg+" - "+exceptionArg+" , "+erlArg;
		}
		
		@Test
		public void testParseInvalidCharsInAtomFail()
		{
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,exception);
			
			if (!erlException.isEmpty())
				checkForCorrectException(new whatToRun() { public @Override void run() {
					ErlangRunner.getRunner().evaluateString(text);
				}},RuntimeException.class,erlException);
		}
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
	public void testParseList4()
	{
		for(String text:new String[]{
				" [ e,267.5E40,45.8]"," [ e, 267.5E40, 45.8 ]"," [e,267.5E40, 45.8]"," [ e , 267.5E40 , 45.8]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse("['e',2.675E42,45.8]",text);
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
	public static class TestParseDoubleFail
	{
		@Parameters
		public static Collection<Object[]> data() 
		{
			final String badmatch = "badmatch"; // it is always so for real Erlang because we'll get an error which correspond to a pattern match failure. 
			return java.util.Arrays.asList(new Object[][]{
					new Object[]{"56R","invalid token",badmatch},
					new Object[]{"56[","invalid token",badmatch},
					new Object[]{"56<","invalid token",badmatch},
					new Object[]{"56.R","invalid token",badmatch},
					new Object[]{"56 .R","unexpected dot",badmatch},
					new Object[]{"56-","invalid token",badmatch},
					new Object[]{"56 -","invalid token",badmatch},
					new Object[]{"56-7","expected dot",""},// Erlang permits this, we do not (expressions are not permitted for us)
					new Object[]{"56.-","invalid token",badmatch},
					new Object[]{"56..","unexpected dot",badmatch},
					new Object[]{"56.-7","expected dot",badmatch},
					new Object[]{"56. 7","unexpected space",badmatch},
					new Object[]{"56.[","invalid token",badmatch},
					new Object[]{"56.<","invalid token",badmatch},
					new Object[]{"56.5R","invalid token",badmatch},
					new Object[]{"56.5[","invalid token",badmatch},
					new Object[]{"56.5<","invalid token",badmatch},
					new Object[]{"56e","unexpected end of",badmatch},
					new Object[]{"56e.","unexpected dot",badmatch},
					new Object[]{"56ee","unexpected exponent",badmatch},
					new Object[]{"56 e","unexpected exponent",badmatch},
					new Object[]{"56.e","unexpected end of","illegal_expr"},
					new Object[]{"56.eR","invalid token","illegal_expr"},
					new Object[]{"56.e[","invalid token",badmatch},
					new Object[]{"56.e<","invalid token",badmatch},
					new Object[]{"56.e-","invalid token",badmatch},
					new Object[]{"56.e-6e","unexpected exponent",badmatch},
					new Object[]{"56.e-6R","invalid token",badmatch},
					new Object[]{"56.e-6[","invalid token",badmatch},
					new Object[]{"56.e-6.","unexpected dot",badmatch},
					new Object[]{"56.e-6<","invalid token",badmatch},
					new Object[]{"56.e -6.","unexpected space",badmatch},
					new Object[]{"56.e ,","invalid token",badmatch},
					new Object[]{"56.e - 6.","invalid token",badmatch},
			});
		}
		
		final String text,exception, erlEx; 
		
		public TestParseDoubleFail(String textArg,String exceptionArg, String erlExArg)
		{
			text = textArg;exception = exceptionArg;erlEx = erlExArg;
		}
		
		public static String parametersToString(String textArg,String exceptionArg, @SuppressWarnings("unused") String erlExArg)
		{
			return textArg+" - "+exceptionArg;
		}
		
		@Test
		public void testDoubleFail()
		{
			checkForCorrectException(new whatToRun() { public @Override void run() {
				ErlangLabel.parseText(text);
			}},IllegalArgumentException.class,exception);

			if (!erlEx.isEmpty())
				checkForCorrectException(new whatToRun() { public @Override void run() {
					ErlangRunner.getRunner().evaluateString(text);
				}},RuntimeException.class,erlEx);
		}
	}

	@RunWith(Parameterized.class)
	public static class TestParseBitStrFail
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
		
		public TestParseBitStrFail(String textArg,String exceptionArg, String erlExArg)
		{
			text = textArg;exception = exceptionArg;erlEx = erlExArg;
		}
		
		public static String parametersToString(String textArg,String exceptionArg, @SuppressWarnings("unused") String erlExArg)
		{
			return textArg+" - "+exceptionArg;
		}
		
		@Test
		public void testBitStrFail()
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
