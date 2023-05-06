package statechum.analysis.Erlang;

import com.ericsson.otp.erlang.*;
import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized.Parameters;
import junit_runners.ParameterizedWithName;
import junit_runners.ParameterizedWithName.ParametersToString;
import statechum.Configuration;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;

import java.util.Collection;

import static statechum.TestHelper.checkForCorrectException;
import static statechum.analysis.Erlang.ErlangLabel.erlBar;

public class TestErlangParser {
	
	/** Runner for the tests. */
	ErlangRunner runner;
	/** The runtime containing Erlang. */
	static ErlangRuntime runtime;
	
	@BeforeClass
	public static void beforeClass()
	{
		runtime = new ErlangRuntime();runtime.setTimeout(100);
		runtime.startRunner();
	}
	
	@AfterClass
	public static void afterClass()
	{
		runtime.killErlang();
	}
	
	@Before
	public void beforeTest()
	{
		runner = runtime.createNewRunner();
	}
	
	@After
	public void afterTest()
	{
		if (runner != null) runner.close();
	}
	
	@Test
	public void testToString1()
	{
		Assert.assertEquals("'this is an atom'",ErlangLabel.dumpErlangObject(new OtpErlangAtom("this is an atom")));
	}
	
	@Test
	public void testToString2()
	{
		Assert.assertEquals("\"this is a string\"",ErlangLabel.dumpErlangObject(new OtpErlangString("this is a string")));
	}
	
	@Test
	public void testToString3()
	{
		Assert.assertEquals("['this is an atom',\"this is a string\"]",
				ErlangLabel.dumpErlangObject(new OtpErlangList(new OtpErlangObject[]{
						new OtpErlangAtom("this is an atom"),
						new OtpErlangString("this is a string")
				})));
	}
	
	@Test
	public void testToString4()
	{
		Assert.assertEquals("{'this is an atom',\"this is a string\"}",
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
		Assert.assertEquals("{'this is an atom',\"this is a string\",[[-234],{}]}",
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
		checkForCorrectException(() -> ErlangLabel.dumpErlangObject(new OtpErlangRef("aa",1,2)),IllegalArgumentException.class,"OtpErlangRef is not supported");
	}
	
	/** Empty term. */
	@Test
	public void testParse0a()
	{
		final String text = "  ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"empty term");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	/** Junk at the end of term. */
	@Test
	public void testParse0b()
	{
		final String text = "'this is an atom' junk";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected characters at the end of string to parse");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	/** Junk at the end of term which can be retrieved. */
	@Test
	public void testParse0c()
	{
		final String text = "'this is an atom' junk and more";
    	Lexer lexer = ErlangLabel.buildLexer(text);
    	OtpErlangObject result = ErlangLabel.parseFirstTermInText(lexer);
		OtpErlangAtom atom = (OtpErlangAtom)result;
		Assert.assertEquals("'this is an atom'",ErlangLabel.dumpErlangObject(atom));
		Assert.assertEquals("junk and more",lexer.remaining());
	}
	
	@Test
	public void testParse1aa()
	{
		String text = "''";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(runner, text,text);
	}
	
	@Test
	public void testParse1a()
	{
		String text = "'this is an atom'";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(runner, text,text);
	}
	
	/** With spaces. */
	@Test
	public void testParse1b()
	{
		String text = "   'this is an atom'";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangAtom);
		checkResponse(runner, text.trim(),text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1c()
	{
		String text = " 'this \\'is an\\\\  \"atom'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is an\\  \"atom",atom.atomValue());
		checkResponse(runner, text.trim(),ErlangLabel.dumpErlangObject(atom));
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1d()
	{
		String text = " 'this \\'is \\\"fh an \"atom'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this 'is \"fh an \"atom",atom.atomValue());
		checkResponse(runner, "'this \\'is \"fh an \"atom'",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1e()
	{
		String text = " 'this \\' is \\\" fh an \"atom'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this ' is \" fh an \"atom",atom.atomValue());
		checkResponse(runner, "'this \\' is \" fh an \"atom'",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse1f()
	{
		String text = " 'this\\'i s\\\" fh an \"atom'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("this'i s\" fh an \"atom",atom.atomValue());
		checkResponse(runner, "'this\\'i s\" fh an \"atom'",text);
	}
	
	/** Spaces. */
	@Test
	public void testParse1g()
	{
		String text = " '  '   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals("  ",atom.atomValue());
		checkResponse(runner, "'  '",text);
	}	
	
	/** Spaces. */
	@Test
	public void testParse1h()
	{
		String text = " ' a '   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(" a ",atom.atomValue());
		checkResponse(runner, "' a '",text);
	}	

	/** Bar 1. */
	@Test
	public void testParse1i()
	{
		String text = " ' | a '   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(" | a ",atom.atomValue());
		checkResponse(runner, "' | a '",text);
	}	

	/** Bar 2. */
	@Test
	public void testParse1j()
	{
		String text = " ' a |'   ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(" a |",atom.atomValue());
		checkResponse(runner, "' a |'",text);
	}

	@Test
	public void testParse1k()
	{
		String text = " ' a =>  ' ";
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseText(text);
		Assert.assertEquals(" a =>  ",atom.atomValue());
		checkResponse(runner, "' a =>  '",text);
	}
	public static void testParseChokesOnInvalidChars(ErlangRunner runner, final char startStop)
	{
		for(String str:new String[]{
				"valid text",
				"some-text",
				"some -text",
				"some- text",
				"some - , text",
				"some-,text",
				"567some-,text",
				"|some",
				"so|me",
				"=>"
		})
		{
			final String data = ""+startStop+str+startStop;
			//OtpErlangObject smth = ErlangLabel.parseText(data);
			// verify that the string can be parsed ok.
			//Assert.assertEquals("parse of "+data+" gave wrong value",data,ErlangLabel.dumpErlangObject(smth));
			checkResponse(runner, data,data);
		}
		
		/* Invalid characters after backslash. */
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
			checkResponse(runner, data.replace(""+startStop+"\\",""+startStop),data);
		}
		for(String str:new String[]{
				"\\text",// this notation is not supported at the moment
				"\\56" // this notation is not supported at the moment
		})
		{
			final String data = str;
			checkForCorrectException(() -> ErlangLabel.parseText(""+startStop+data+startStop),IllegalArgumentException.class,"is not supposed to be prefixed");
			
			// We are a lot more restrictive here compared to the actual Erlang runtime - all of the above 
			// goes through fine.
			runner.evaluateString(""+startStop+data+startStop);
		}
		
	}
	
	/** Invalid characters after backslash. */
	@Test
	public void testParse1Fail1()
	{
		final String text = "'this is an atom\\\" ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of atom");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
		
	@Test
	public void testParse1Fail2()
	{
		testParseChokesOnInvalidChars(runner, '\'');
	}
	
	@Test
	public void testParses2a()
	{
		String text = "\"this is a string\"";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangString);
		checkResponse(runner, text,text);
	}
	
	/** With spaces. */
	@Test
	public void testParse2b()
	{
		String text = "   \"this is a \\\\ string\"";
		Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangString);
		checkResponse(runner, text.trim(),text);
	}

	@Test
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
		String text = " \"this \\\"is \\'fh an 'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \"is 'fh an 'atom",string.stringValue());
		checkResponse(runner, "\"this \\\"is 'fh an 'atom\"",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2e()
	{
		String text = " \"this \\\" is \\' fh an 'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this \" is ' fh an 'atom",string.stringValue());
		checkResponse(runner, "\"this \\\" is ' fh an 'atom\"",text);
	}
	
	/** Quoted characters. */
	@Test
	public void testParse2f()
	{
		String text = " \"this\\\"i s\\' fh an 'atom\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("this\"i s' fh an 'atom",string.stringValue());
		checkResponse(runner, "\"this\\\"i s' fh an 'atom\"",text);
	}
	
	/** Spaces. */
	@Test
	public void testParse2g()
	{
		String text = " \"  \"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals("  ",string.stringValue());
		checkResponse(runner, "\"  \"",text);
	}	
	
	/** Spaces. */
	@Test
	public void testParse2h()
	{
		String text = " \" a \"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(" a ",string.stringValue());
		checkResponse(runner, "\" a \"",text);
	}	

	/** Bar 1. */
	@Test
	public void testParse2i()
	{
		String text = " \" | a \"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(" | a ",string.stringValue());
		checkResponse(runner, "\" | a \"",text);
	}	

	/** Bar 2. */
	@Test
	public void testParse2j()
	{
		String text = "  \" a |\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(" a |",string.stringValue());
		checkResponse(runner, "\" a |\"",text);
	}

	@Test
	public void testParse2k()
	{
		String text = "  \" a => b\"   ";
		OtpErlangString string = (OtpErlangString)ErlangLabel.parseText(text);
		Assert.assertEquals(" a => b",string.stringValue());
		checkResponse(runner, "\" a => b\"",text);
	}

	/** Invalid characters after backslash. */
	@Test
	public void testParse2Fail1()
	{
		final String text = "\"this is an atom\\\" ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of string");
	}
		
	@Test
	public void testParse2Fail2()
	{
		testParseChokesOnInvalidChars(runner, '\"');
	}
	
	/** Checks that both our parser and Erlang runtime produce the same response to parsing the supplied chunk of text. */
	protected static void checkResponse(ErlangRunner runner, String expectedOutcome, String text)
	{
		Assert.assertEquals(expectedOutcome,ErlangLabel.dumpErlangObject(ErlangLabel.parseText(text)));
		Assert.assertEquals(expectedOutcome,ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse3()
	{
		String text = " -45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(-45,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse4()
	{
		String text = " 45 ";
		OtpErlangInt number = (OtpErlangInt)ErlangLabel.parseText(text);
		Assert.assertEquals(45,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}

	@Test
	public void testParse4fail1()
	{
		final String text = " 45junk";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"invalid token type");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParse4fail2()
	{
		final String text = " 45 junk";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"in parsing erlang number");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParse4fail4a()
	{
		final String text = " 45 =>";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected characters at the end of string to parse, looking at =>");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParse4fail4b()
	{
		final String text = " 45=>";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected characters at the end of string to parse, looking at =>");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParse4fail5()
	{
		final String text = "  => 4";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"in parsing erlang term");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParse5a()
	{
		String text = " 45888888888";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(45888888888L,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse5b()
	{
		String text = " -458";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(-458L,number.longValue());
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse5c()
	{
		String text = " +458";
		OtpErlangLong number = (OtpErlangLong)ErlangLabel.parseText(text);
		Assert.assertEquals(458L,number.longValue());
		Assert.assertEquals("458",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6a()
	{
		String text = " 458888.5";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(458888.5,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("458888.5",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6b()
	{
		String text = " 458888.5e0 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(458888.5,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("458888.5",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6c()
	{
		String text = " 458888.5e-5 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588885,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588885",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6d()
	{
		String text = " 458888.5e5 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588885e10,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588885E10",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6e()
	{
		String text = " 4588.456e34 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588456e37,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588456E37",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6f()
	{
		String text = " 4588888888888888.456e244 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588888888888889E259,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588888888888889E259",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6g()
	{
		String text = " 4588888888888888.456e-244 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4.588888888888888E-229,number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals("4.588888888888888E-229",ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
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
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6hb()
	{
		final String text = " 4.0e"+(-ErlangLabel.ErlangLong.minExponent-1)+" ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"cannot be represented");
		//try
		{
			Assert.assertEquals(0,((OtpErlangDouble)runner.evaluateString(text)).doubleValue(),Configuration.fpAccuracy);
			// if the exception is not thrown but the assertion is satisfied, this is fine (this is what happens on WinXP).
		}
		/*
		catch(Exception ex)
		{// On Linux with a broken Erlang OTP14 (broken by Debian), an exception is thrown.
			Class<RuntimeException> exceptionClass = RuntimeException.class;
			String exceptionString = "badmatch";
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong type of exception received "+str.toString()+" instead of "+exceptionClass,exceptionClass,ex.getClass());
			Assert.assertTrue("expected exception containing \""+exceptionString+"\" but got \""+ex.getMessage()+"\"",ex.getMessage().contains(exceptionString));
		}
		*/
	}
	
	@Test
	public void testParse6ia()
	{
		final String text = " 4.0E"+(ErlangLabel.ErlangLong.maxExponent)+" ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4d*Math.pow(10, ErlangLabel.ErlangLong.maxExponent),number.doubleValue(),Configuration.fpAccuracy);
		Assert.assertEquals(text.trim(),ErlangLabel.dumpErlangObject(runner.evaluateString(text)));
	}
	
	@Test
	public void testParse6ib()
	{
		final String text = " 4.0e"+(ErlangLabel.ErlangLong.maxExponent+1)+" ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"cannot be represented");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@SuppressWarnings("StatementWithEmptyBody")
	@Test
	public void testCheckMaxExponent()
	{
		int i;
		for(i=200;i< 400 &&	!Double.isInfinite(Double.parseDouble("4.0e"+i));++i)
		{}
		Assert.assertEquals(ErlangLabel.ErlangLong.maxExponent+1, i);
	}

	@SuppressWarnings("StatementWithEmptyBody")
	@Test
	public void testCheckMinExponent()
	{
		int i;
		for(i=-200;i> -400 && 0 !=(Double.parseDouble("4.0e"+i));--i)
		{}
		Assert.assertEquals(ErlangLabel.ErlangLong.minExponent+1, -i);
	}

	/** Erlang refuses to parse 4e10, I think this is wrong and hence my parser accepts this. */
	@Test
	public void testParse6j()
	{
		final String text = " 4e10 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4e10,number.doubleValue(),Configuration.fpAccuracy);
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	/** Erlang refuses to parse 4.e10, I think this is wrong hence my parser accepts this. */
	@Test
	public void testParse6k()
	{
		final String text = " 4.e10 ";
		OtpErlangDouble number = (OtpErlangDouble)ErlangLabel.parseText(text);
		Assert.assertEquals(4e10,number.doubleValue(),Configuration.fpAccuracy);
		try
		{
			runner.evaluateString(text);
		}
		catch(Exception ex)
		{
			Assert.assertEquals(IllegalArgumentException.class, ex.getClass());
			Assert.assertTrue(ex.getMessage().contains("badmatch") || ex.getMessage().contains("illegal_expr"));// Erlang 16 returns badmatch, 14 and 15 return "illegal_expr"
		}
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
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"is never allowed in an atom");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	/** Invalid characters after backslash. */
	@Test
	public void testParseUnquotedAtomFail2()
	{
		final String text = "thi\\\"s ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"is never allowed in an atom");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseUnquotedAtomFail3()
	{
		final String text = " aa junk";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected characters at the end of string to parse");
		OtpErlangAtom atom = (OtpErlangAtom)ErlangLabel.parseFirstTermInText(ErlangLabel.buildLexer(text));
		Assert.assertEquals("aa",atom.atomValue());
	}
	
	/** Atom cannot begin with a dot. */
	@Test
	public void testParseUnquotedAtomFail6()
	{
		final String text = " .aa junk";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"cannot start with a dot");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseUnquotedAtomFail7()
	{
		final String text = " junk=>";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unquoted atom parser: > is never allowed");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseTuple1()
	{
		for(String text:new String[]{
				" { a, b}"," { a, b }"," {a, b}"," {a,b}"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse(runner, "{'a','b'}",text);
		}
	}
	
	@Test
	public void testParseTuple2()
	{
		for(String text:new String[]{
				" { }"," {}","{}"," { } "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse(runner, "{}",text);
		}
	}
	
	@Test
	public void testParseTuple3()
	{
		for(String text:new String[]{
				" { a }"," {a}","{a }"," {  a} "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangTuple);
			checkResponse(runner, "{'a'}",text);
		}
	}
	
	@Test
	public void testParseTuple1Fail1()
	{
		final String text = "{a";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail2()
	{
		final String text = "{a{";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail3()
	{
		final String text = "{a{}";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail4()
	{
		final String text = "{a,";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail5()
	{
		final String text = "{a{ ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail6()
	{
		final String text = "{a , ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail7()
	{
		final String text = "{,";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma in parsing tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail8()
	{
		final String text = "{ , ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma in parsing tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail9()
	{
		final String text = "{ 56, hh";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseTuple1Fail10()
	{
		final String text = "{ 56  |";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"invalid token type "+ erlBar);
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseTuple1Fail11()
	{
		final String text = "{ 56 =>";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected => in parsing tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseTuple1Fail12()
	{
		final String text = "{ 56=>";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected => in parsing tuple");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	/** invalid atoms. */
	@RunWith(ParameterizedWithName.class)
	public static class TestParseInvalidCharsInAtomFail
	{
		/** Runner for the tests. */
		ErlangRunner runner;
		/** The runtime containing Erlang. */
		static ErlangRuntime testParseInvalidCharsInAtomRuntime;
		
		@BeforeClass
		public static void beforeClass()
		{
			testParseInvalidCharsInAtomRuntime = new ErlangRuntime();testParseInvalidCharsInAtomRuntime.setTimeout(100);testParseInvalidCharsInAtomRuntime.startRunner();
		}
		
		@AfterClass
		public static void afterClass()
		{
			testParseInvalidCharsInAtomRuntime.killErlang();
		}
		
		@Before
		public void beforeTest()
		{
			runner = testParseInvalidCharsInAtomRuntime.createNewRunner();
		}
		
		@After
		public void afterTest()
		{
			runner.close();
		}

		@Parameters
		public static Collection<Object[]> data() 
		{
			final String unquotedError = "unquoted atom cannot start with a dot",
			isNeverAllowedError = "is never allowed",invalidTokenError = "invalid token", unexpectedMapSep = "unexpected => in parsing",
			erlEvaluation = "evaluation",
			erlNone = "";// this means 'ignore what Erlang says'.
			return java.util.Arrays.asList(new Object[]{"{ 56, .aa }",unquotedError,erlNone},// Real Erlang accepts this but throws away the dot - I'm not happy about this.
					new Object[]{"{ 56, +aa }",invalidTokenError,erlEvaluation},
					new Object[]{"{ 56, -aa }",invalidTokenError,erlEvaluation},
					new Object[]{"[ 56, .aa ]",unquotedError,erlNone},
					new Object[]{"[ 56, +aa ]",invalidTokenError,erlEvaluation},
					new Object[]{"[ 56, -aa ]",invalidTokenError,erlEvaluation},
					new Object[]{"{ 56, [.aa] }",unquotedError,erlNone},
					new Object[]{"{ 56,[ +aa ]}",invalidTokenError,erlEvaluation},
					new Object[]{"{ 56, [-aa ]}",invalidTokenError,erlEvaluation},
					new Object[]{"[ 56, {.aa} ]",unquotedError,erlNone},
					new Object[]{"[ 56,{ +aa} ]",invalidTokenError,erlEvaluation},
					new Object[]{"[ 56, {-aa }]",invalidTokenError,erlEvaluation},

					// Here bar cannot be a part of an atom hence the error is proper,
					// albeit it is reported by the list parser and not by atom parser.
					new Object[]{"[ 56, | aa]","unexpected bar",erlEvaluation},
					new Object[]{"+ [ 56 ]",invalidTokenError,erlEvaluation},
					new Object[]{". [ 56 ]",unquotedError,erlEvaluation},
					new Object[]{"- [ 56 ]",invalidTokenError,erlEvaluation},
					new Object[]{"+[ 56 ]",invalidTokenError,erlEvaluation},
					new Object[]{".[ 56 ]",unquotedError,erlEvaluation},
					new Object[]{"-[ 56 ]",invalidTokenError,erlEvaluation},
					new Object[]{"|[56]",invalidTokenError,erlEvaluation},

					new Object[]{"a-atom",isNeverAllowedError,erlEvaluation},
					new Object[]{"a:atom",isNeverAllowedError,erlEvaluation},
					new Object[]{"atom-a",isNeverAllowedError,erlEvaluation},
					new Object[]{"atom:b",isNeverAllowedError,erlEvaluation},
					new Object[]{"at|om","unexpected characters",erlEvaluation},

					new Object[]{"{ 56 => 5 }",unexpectedMapSep,erlEvaluation},
					new Object[]{"{ 56=> 5 }",unexpectedMapSep,erlEvaluation},
					new Object[]{"{ 56 => }",unexpectedMapSep,erlEvaluation},
					new Object[]{"{ => , }",unexpectedMapSep,erlEvaluation},
					new Object[]{"{ , =>  }","unexpected comma in parsing tuple",erlEvaluation},

					new Object[]{"[ 56 => 5 ]",unexpectedMapSep,erlEvaluation},
					new Object[]{"[ 56=> 5 ]",unexpectedMapSep,erlEvaluation},
					new Object[]{"[ 56 => ]",unexpectedMapSep,erlEvaluation},
					new Object[]{"[ => , ]",unexpectedMapSep,erlEvaluation},
					new Object[]{"[ , =>  ]","unexpected comma in parsing list",erlEvaluation},

					new Object[]{"-atom",invalidTokenError,erlEvaluation},
					new Object[]{"+atom",invalidTokenError,erlEvaluation},
					new Object[]{":atom",invalidTokenError,erlEvaluation},
					new Object[]{">atom",invalidTokenError,erlEvaluation},
					new Object[]{"<atom",invalidTokenError,erlEvaluation},
					new Object[]{"|atom",invalidTokenError,erlEvaluation});
		}
		final String text,exception,erlException; 
					
		public TestParseInvalidCharsInAtomFail(String textArg,String exceptionArg, String erlArg)
		{
			text = textArg;exception = exceptionArg;erlException = erlArg;
		}
		
		@ParametersToString
		public static String parametersToString(String textArg,String exceptionArg, String erlArg)
		{
			return textArg+" - "+exceptionArg+" , "+erlArg;
		}
		
		@Test
		public void testParseInvalidCharsInAtomFail()
		{
			checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,exception);
			
			if (!erlException.isEmpty())
				checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,erlException);
		}
	}
	
	@Test
	public void testParseList1()
	{
		for(String text:new String[]{
				" [ a, b]"," [ a, b ]"," [a, b]"," [a,b]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b']",text);
		}
	}
	
	@Test
	public void testParseList2()
	{
		for(String text:new String[]{
				" [ ]"," []","[]"," [ ] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "[]",text);
		}
	}
	
	@Test
	public void testParseList3()
	{
		for(String text:new String[]{
				" [ a ]"," [a]","[a ]"," [  a] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a']",text);
		}
	}

	@Test
	public void testParseList4()
	{
		for(String text:new String[]{
				" [ e,267.5E40,45.8]"," [ e, 267.5E40, 45.8 ]"," [e,267.5E40, 45.8]"," [ e , 267.5E40 , 45.8]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['e',2.675E42,45.8]",text);
		}
	}
	
	@Test
	public void testParseList5()
	{
		for(String text:new String[]{
				" [ a,b,c | d]"," [a|[b|[c|d]]]","[a,b|[c|d] ]"," [  a, b | [ c | d ] ] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b','c' | 'd']",text);
		}
	}

	
	@Test
	public void testParseList6()
	{
		for(String text:new String[]{
				" [ a,b,c,d]"," [a|[b|[c|[d]]]]","[a,b|[c|[d]] ]"," [  a, b| [ c | [d] ] ] "})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b','c','d']",text);
		}
	}

	@Test
	public void testParseList7()
	{
		for(String text:new String[]{
				" [ a,b,40| -5 ]"," [a|[b|[40|-5]]]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b',40 | -5]",text);
		}
	}

	/** Erlang rejects integers in scientific notation such as 7e4, it has to be 7.0e4.
	 * We also happily parse 7.e4 which Erlang rejects.
	  */
	@Test
	public void testParseList8()
	{
		for(String text:new String[]{
				" [ a,b,7.0e4| -5 ]"," [a|[b|[7.0e4|-5]]]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b',70000.0 | -5]",text);
		}
	}

	@Test
	public void testParseList9()
	{
		for(String text:new String[]{
				" [ a,b,c| \"test\" ]"," [a|[b|[c|[116,101,115,116]]]]"})
		{
			Assert.assertTrue( ErlangLabel.parseText(text) instanceof OtpErlangList);
			checkResponse(runner, "['a','b','c',116,101,115,116]",text);
		}
	}

	@Test
	public void testParseList1Fail1()
	{
		final String text = "[a";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail2()
	{
		final String text = "[a[";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail3()
	{
		final String text = "[a{}";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail4()
	{
		final String text = "[a,";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail5()
	{
		final String text = "[a{ ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail6()
	{
		final String text = "[a , ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail7()
	{
		final String text = "[,";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail8()
	{
		final String text = "[ , ";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail9a()
	{
		final String text = "[ 56, hh";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseList1Fail9b()
	{
		final String text = "[ 56=> hh";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected => in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseList1Fail9c()
	{
		final String text = "[ 56 => hh";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected => in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}

	@Test
	public void testParseList1Fail10()
	{
		final String text = "[ |";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected bar in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail11()
	{
		final String text = "[ a |";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail12()
	{
		final String text = "[ a |,";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail13()
	{
		final String text = "[ a |]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"missing tail in improper list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail14()
	{
		final String text = "[ a |[,]]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail15()
	{
		final String text = "[ a |[5],]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected comma");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail16()
	{
		final String text = "[ a |[5]6]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting comma in parsing list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail17()
	{
		final String text = "[ a |[5|]]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"missing tail in improper list");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseList1Fail18()
	{
		final String text = "[ a |[5] | ]";
		checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected bar");
		checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
	}
	
	@Test
	public void testParseListAndTupleBig()
	{
		final String text = "{'this is an atom',\"this is a string\",[[-234],{}]}";
		OtpErlangObject obtained = ErlangLabel.parseText(text);
		checkResponse(runner, text, text);
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
		OtpErlangBitstr str = (OtpErlangBitstr) runner.evaluateString("<<  >>");
		Assert.assertEquals("<< >>",ErlangLabel.dumpErlangObject(str));
	}
	
	@Test
	public void testDumpBitStr1()
	{
		OtpErlangBitstr str = (OtpErlangBitstr) runner.evaluateString("<< 1:4,0:4/integer-unit:1,1:1,0:7 >>");
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
		final OtpErlangBitstr str = (OtpErlangBitstr) runner.evaluateString("<< 1:4,0:4/integer-unit:1,1:1 >>");
		checkForCorrectException(() -> ErlangLabel.dumpErlangObject(str),IllegalArgumentException.class,"is not divisible by 8");
	}
	
	@RunWith(ParameterizedWithName.class)
	public static class TestParseDoubleFail
	{
		/** Runner for the tests. */
		ErlangRunner runner;
		/** The runtime containing Erlang. */
		static ErlangRuntime testParseDoubleFailRuntime;
		
		@BeforeClass
		public static void beforeClass()
		{
			testParseDoubleFailRuntime = new ErlangRuntime();testParseDoubleFailRuntime.setTimeout(100);testParseDoubleFailRuntime.startRunner();
		}
		
		@AfterClass
		public static void afterClass()
		{
			testParseDoubleFailRuntime.killErlang();
		}
		
		@Before
		public void beforeTest()
		{
			runner = testParseDoubleFailRuntime.createNewRunner();
		}
		
		@After
		public void afterTest()
		{
			runner.close();
		}

		@Parameters
		public static Collection<Object[]> data() 
		{
			final String badmatch = "badmatch"; // it is always so for real Erlang because we'll get an error which correspond to a pattern match failure. 
			return java.util.Arrays.asList(new Object[]{"56R","invalid token",badmatch,""},
					new Object[]{"56[","invalid token",badmatch,""},
					new Object[]{"56<","invalid token",badmatch,""},
					new Object[]{"56.R","invalid token",badmatch,""},
					new Object[]{"56 .R","unexpected dot",badmatch,""},
					new Object[]{"56 |","unexpected characters at the end",badmatch,""},
					new Object[]{"56|","unexpected characters at the end",badmatch,""},
					new Object[]{"56-","invalid token",badmatch,""},
					new Object[]{"56 -","invalid token",badmatch,""},
					new Object[]{"56-7","expected dot","",""},// Erlang permits this, we do not (expressions are not permitted for us)
					new Object[]{"56-|","invalid token",badmatch,""},
					new Object[]{"56.-","invalid token",badmatch,""},
					new Object[]{"56..","unexpected dot",badmatch,""},
					new Object[]{"56.-7","expected dot",badmatch,""},
					new Object[]{"56. 7","unexpected space",badmatch,""},
					new Object[]{"56.[","invalid token",badmatch,""},
					new Object[]{"56.<","invalid token",badmatch,""},
					new Object[]{"56.|","unexpected characters at the end",badmatch,""},
					new Object[]{"56.5R","invalid token",badmatch,""},
					new Object[]{"56.5[","invalid token",badmatch,""},
					new Object[]{"56.5<","invalid token",badmatch,""},
					new Object[]{"56.5|","unexpected characters at the end",badmatch,""},
					new Object[]{"56e","unexpected end of",badmatch,""},
					new Object[]{"56e.","unexpected dot",badmatch,""},
					new Object[]{"56ee","unexpected exponent",badmatch,""},
					new Object[]{"56e|","invalid token",badmatch,""},
					new Object[]{"56 e","unexpected exponent",badmatch,""},
					new Object[]{"56 |","unexpected characters at the end",badmatch,""},
					new Object[]{"56.e","unexpected end of","illegal_expr",badmatch},
					new Object[]{"56.eR","invalid token","illegal_expr",badmatch},
					new Object[]{"56.e[","invalid token",badmatch,""},
					new Object[]{"56.e<","invalid token",badmatch,""},
					new Object[]{"56.e|","invalid token",badmatch,""},
					new Object[]{"56.e-","invalid token",badmatch,""},
					new Object[]{"56.e-|","invalid token",badmatch,""},
					new Object[]{"56.e-6e","unexpected exponent",badmatch,""},
					new Object[]{"56.e-6R","invalid token",badmatch,""},
					new Object[]{"56.e-6[","invalid token",badmatch,""},
					new Object[]{"56.e-6.","unexpected dot",badmatch,""},
					new Object[]{"56.e-6<","invalid token",badmatch,""},
					new Object[]{"56.e-6|","unexpected characters at the end",badmatch,""},
					new Object[]{"56.e -6.","unexpected space",badmatch,""},
					new Object[]{"56.e ,","invalid token",badmatch,""},
					new Object[]{"56.e |","invalid token",badmatch,""},
					new Object[]{"56.e - 6.","invalid token",badmatch,""},
					new Object[]{"56.e - 6|","invalid token",badmatch,""});
		}
		
		final String text,exception, errOne, errTwo; 
		
		public TestParseDoubleFail(String textArg,String exceptionArg, String erlExArg1, String erlExArg2)
		{
			text = textArg;exception = exceptionArg;errOne = erlExArg1;errTwo = erlExArg2;
		}
		
		@ParametersToString
		public static String parametersToString(String textArg,String exceptionArg, @SuppressWarnings("unused") String erlExArg1, @SuppressWarnings("unused") String erlExArg2)
		{
			return textArg+" - "+exceptionArg;
		}
		
		@Test
		public void testDoubleFail()
		{
			checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,exception);

			if (!errOne.isEmpty())
			{
				try
				{
					runner.evaluateString(text);
				}
				catch(Exception ex)
				{
					Assert.assertEquals(IllegalArgumentException.class, ex.getClass());
					Assert.assertTrue( (!errOne.isEmpty() && ex.getMessage().contains(errOne))  || (!errTwo.isEmpty() && ex.getMessage().contains(errTwo)));// Erlang 16 returns badmatch, 14 and 15 return "illegal_expr"
				}
			}
		}
	}

	@RunWith(ParameterizedWithName.class)
	public static class TestParseBitStrFail
	{
		/** Runner for the tests. */
		ErlangRunner runner;
		/** The runtime containing Erlang. */
		static ErlangRuntime testParseRuntime;
		
		@BeforeClass
		public static void beforeClass()
		{
			testParseRuntime = new ErlangRuntime();testParseRuntime.setTimeout(100);testParseRuntime.startRunner();
		}
		
		@AfterClass
		public static void afterClass()
		{
			testParseRuntime.killErlang();
		}
		
		@Before
		public void beforeTest()
		{
			runner = testParseRuntime.createNewRunner();
		}
		
		@After
		public void afterTest()
		{
			runner.close();
		}

		@Parameters
		public static Collection<Object[]> data() 
		{
			final String badmatch = "badmatch"; // it is always so for real Erlang because we'll get an error which correspond to a pattern match failure. 
			return java.util.Arrays.asList(new Object[]{"<<","unexpected end of",badmatch},
					new Object[]{"<< <","invalid token",badmatch},
					new Object[]{"<< <<","invalid token",badmatch},
					new Object[]{"<< >","invalid token",badmatch},
					new Object[]{"<< ,","got , in NUM_OR_END",badmatch},
					new Object[]{"<< :","got : in NUM_OR_END",badmatch},
					new Object[]{"<< a","type is unexpected",badmatch},
					new Object[]{"<< -","dash can only be used after a type",badmatch},
					new Object[]{"<< /","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< |","invalid token",badmatch},

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
					new Object[]{"<< -56|","invalid token",badmatch},

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
					new Object[]{"<< -56:|","invalid token",badmatch},

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
					new Object[]{"<< -56:34|","invalid token",badmatch},
					new Object[]{"<< -56:34 |","invalid token",badmatch},
					new Object[]{"<< -56:34| ","invalid token",badmatch},

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
					new Object[]{"<< -56:34/|","invalid token",badmatch},

					new Object[]{"<< -56:34/unit,","got , in UCOLON",badmatch},
					new Object[]{"<< -56:34/unit >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/unit|","invalid token",badmatch},

					new Object[]{"<< -56:34/signed,","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed >","invalid token",badmatch},
					new Object[]{"<< -56:34/signed -","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed-","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/signed>","invalid token",badmatch},
					new Object[]{"<< -56:34/signed<","invalid token",badmatch},
					new Object[]{"<< -56:34/signed<<","invalid token",badmatch},
					new Object[]{"<< -56:34/signed/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/signed:","got : in MINUS_COMMA",badmatch},
					new Object[]{"<< -56:34/signed|","invalid token",badmatch},

					new Object[]{"<< -56:34/unit:,","got , in UNIT",badmatch},
					new Object[]{"<< -56:34/unit: >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit: -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit::","got : in UNIT",badmatch},
					new Object[]{"<< -56:34/unit:|","invalid token",badmatch},
					new Object[]{"<< -56:34/unit: |","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:| ","invalid token",badmatch},

					new Object[]{"<< -56:34/unit:5,","unexpected end of bit string",badmatch},
					new Object[]{"<< -56:34/unit:5 >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5 -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:5:","got : in COMMA_END",badmatch},
					new Object[]{"<< -56:34/unit:5|","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5 |","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5| ","invalid token",badmatch},

					new Object[]{"<< -56:34/unit:5,,","got , in NUM",badmatch},
					new Object[]{"<< -56:34/unit:5, >","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5, -","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5,-","dash can only be used after a type",badmatch},
					new Object[]{"<< -56:34/unit:5,>","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,<<","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,/","slash can only be used after NUMBER:SIZE",badmatch},
					new Object[]{"<< -56:34/unit:5,:","got : in NUM",badmatch},
					new Object[]{"<< -56:34/unit:5,|","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5, |","invalid token",badmatch},
					new Object[]{"<< -56:34/unit:5,| ","invalid token",badmatch});
		}
		
		final String text,exception, erlEx; 
		
		public TestParseBitStrFail(String textArg,String exceptionArg, String erlExArg)
		{
			text = textArg;exception = exceptionArg;erlEx = erlExArg;
		}
		
		@ParametersToString
		public static String parametersToString(String textArg,String exceptionArg, @SuppressWarnings("unused") String erlExArg)
		{
			return textArg+" - "+exceptionArg;
		}
		
		@Test
		public void testBitStrFail()
		{
			checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,exception);
			checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,erlEx);
		}
		
	}
	
	// More BigStrParser tests
	
	@Test
	public void testEmpty()
	{
		final String text = "<< >>";
		checkResponse(runner, text, text);
	}
	@Test
	public void testSingleByte()
	{
		final String text = "<< 56 >>";
		checkResponse(runner, "<< 56>>", text);
	}
	
	@Test
	public void testTwoBytes()
	{
		final String text = "<<-89,34 >>";
		checkResponse(runner, "<< 167, 34>>", text);
	}
	@Test
	public void testPattern1()
	{
		final String text = "<< 1:2,32:6 >>";
		checkResponse(runner, "<< 96>>", text);
	}
	
	@Test
	public void testBitStrFail2()
	{
		checkForCorrectException(() -> ErlangLabel.parseText("<< 1:2,32:6,1:1>>"),IllegalArgumentException.class,"should be divisible by 8");
	}
	
	@Test
	public void testPattern2()
	{
		final String text = "<< 1:2,32:6,1:1,0:7 >>";
		checkResponse(runner, "<< 96, 128>>", text);
		//System.out.println(Arrays.toString(new BigInteger(""+(256*128+88)).toByteArray()));
	}
	
	@Test
	public void testPattern3a()
	{
		final String text = "<< 1:2,1:2,0:4 >>";
		checkResponse(runner, "<< 80>>", text);
	}
	
	@Test
	public void testPattern3b()
	{
		final String text = "<< 1:2/big-unit:2,2:4 >>";
		checkResponse(runner, "<< 18>>", text);
	}
	
	@Test
	public void testPattern4a()
	{
		final String text = "<< 1:16 >>";
		checkResponse(runner, "<< 0, 1>>", text);
	}
	
	@Test
	public void testPattern4b()
	{
		final String text = "<< 1:15,1:1 >>";
		checkResponse(runner, "<< 0, 3>>", text);
	}
	
	@Test
	public void testPattern4c()
	{
		final String text = "<< 1:4,1:11,1:1 >>";
		checkResponse(runner, "<< 16, 3>>", text);
	}
	
	@Test
	public void testPattern5a()
	{
		final String text = "<< 1:2,32:6 >>";
		checkResponse(runner, "<< 96>>", text);
	}
	
	@Test
	public void testPattern5b()
	{
		final String text = "<< 1:2,32:6,1:1,0:5,0:2 >>";
		checkResponse(runner, "<< 96, 128>>", text);
	}
	
	@Test
	public void testPattern5c()
	{
		final String text = "<< 1:1,5:5,0:2 >>";
		checkResponse(runner, "<< 148>>", text);
	}
	
	@Test
	public void testPattern5d()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,0:10 >>";
		checkResponse(runner, "<< 96, 148, 0>>", text);
	}
	
	@Test
	public void testPattern5e()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:10 >>";
		checkResponse(runner, "<< 96, 151, 178>>", text);
	}
	
	@Test
	public void testPattern6a()
	{
		final String text = "<< 1:1,5:5,-78:13,3456454:13 >>";
		checkResponse(runner, "<< 151, 246, 93, 198>>", text);
	}
	
	@Test
	public void testPattern6b()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,3456454:13 >>";
		checkResponse(runner, "<< 96, 151, 246, 93, 198>>", text);
	}
	
	@Test
	public void testPattern6c()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,567654565765454:30,3456454:13,3:2 >>";
		checkResponse(runner, "<< 96, 151, 246, 80, 95, 176, 167, 119, 27>>", text);
	}
	
	@Test
	public void testPattern7a()
	{
		final String text = "<< 2:1/little-unit:16 >>";
		checkResponse(runner, "<< 2, 0>>", text);
	}

	@Test
	public void testPattern7b()
	{
		final String text = "<< 567654565765454:32/little >>";
		checkResponse(runner, "<< 78, 97, 191, 96>>", text);
	}

	@Test
	public void testPattern7c()
	{
		final String text = "<< 567654565765454:32/little,128 >>";
		checkResponse(runner, "<< 78, 97, 191, 96, 128>>", text);
	}
	
	@Test
	public void testPattern7d()
	{
		final String text = "<< 256:9/little,64:7 >>";
		checkResponse(runner, "<< 0, 192>>", text);
	}

	@Test
	public void testPattern7e()
	{
		final String text = "<< 256:9/little,-4:31 >>";
		checkResponse(runner, "<< 0, 255, 255, 255, 252>>", text);
	}

	@Test
	public void testPattern7g()
	{
		final String text = "<< 1:1,2:15/little>>";
		checkResponse(runner, "<< 129, 0>>", text);
	}

	@Test
	public void testPattern7f()
	{
		final String text = "<< 1:1,1:15/little>>";
		checkResponse(runner, "<< 128, 128>>", text);
	}

	@Test
	public void testPattern7h()
	{
		final String text = "<< -78:12,567654565765454:28/little >>";
		checkResponse(runner, "<< 251, 36, 230, 27, 240>>", text);
	}
	
	@Test
	public void testPattern7i()
	{
		final String text = "<< 1:2,32:6,1:1,5:5,-78:13,567654565765454:30/little,3456454:13,3:2 >>";
		checkResponse(runner, "<< 96, 151, 246, 73, 204, 55, 240, 119, 27>>", text);
	}
	
}
