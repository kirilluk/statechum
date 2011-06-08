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
 * 
 */
package statechum.analysis.Erlang.Signatures;

import static statechum.Helper.checkForCorrectException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangPort;
import com.ericsson.otp.erlang.OtpErlangString;

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.Helper.whatToRun;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.ErlangRunner;
import statechum.analysis.Erlang.OTPBehaviour;
import statechum.analysis.Erlang.TestErlangRunner;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;

/** This one tests two things, parsing of Erlang types and verification of type compatibility. */
public class TestTypes 
{
	/** The name of test file - should not be static to ensure it picks the value of TestErlangRunner's variable
     * after it has been initialised.
     */
    protected final String erlangFile = TestErlangRunner.testDir.getAbsolutePath()+File.separator+"testFile.erl";
 
    @Before
	public void beforeTest()
	{
		if (!TestErlangRunner.testDir.isDirectory()) 
			Assert.assertTrue("could not create "+TestErlangRunner.testDir.getAbsolutePath(),TestErlangRunner.testDir.mkdir());
		ErlangModule.flushRegistry();
	}
	
	@After
	public void afterTest()
	{
		ExperimentRunner.zapDir(TestErlangRunner.testDir);
	}

	
	/** Creates a label from the string of text and a configuration. */
	public static void createLabel(String str, Configuration config)
	{
		AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+","+str+"}", config);
	}
	
	/** Verifies that the specified label does not parse give the configuration. */
	public static void checkFailureFor(String str,Configuration config)
	{
		try
		{
			createLabel(str, config);
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			if (!ex.getMessage().contains("not type-compatible"))
				Helper.throwUnchecked("unexpected exception received", ex);
		}
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue1()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Tuple',[],[{'Atom',[],['noreply']},{'Any',[]}]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Any',[]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue2()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Tuple',[],[{'Atom',[],['noreply']},{'Alt',[],[{'Atom',[],['stop']},{'Atom',[],['normal']}]}]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Alt',[],[{'Atom',[],['stop']},{'Atom',[],['normal']}]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue3()
	{
		String nestedAlt = "{'Alt',[],[{'Atom',[],['normal']},{'Atom',[],['aaa']},{'Atom',[],['bb']}]}";
		OtpErlangObject obj = ErlangLabel.parseText("{'Tuple',[],[{'Atom',[],['noreply']},{'Alt',[],[{'Atom',[],['stop']},"+nestedAlt+"]}]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Alt',[],[{'Atom',[],['stop']},{'Atom',[],['normal']},{'Atom',[],['aaa']},{'Atom',[],['bb']}]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue4()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Any',[]}]},{'Tuple',[],[{'Atom',[],['stop']},{'Atom',[],['normal']},{'Atom',[],['stopped']}]}]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Alt',[],[{'Any',[]},{'Atom',[],['normal']}]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue5a()
	{
		String nestedTerm = "{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Any',[]}]},{'Tuple',[],[{'Atom',[],['stop']},{'Atom',[],['normal']},{'Atom',[],['stopped']}]}]}";
		OtpErlangObject obj = ErlangLabel.parseText("{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Atom',[],['tmp']}]},"+nestedTerm+"]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Alt',[],[{'Atom',[],['tmp']},{'Any',[]},{'Atom',[],['normal']}]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values, here the nested tuple is too short. */
	@Test
	public void testExtractionOfOTPreturnValue5b()
	{
		String nestedTerm = "{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Any',[]}]},{'Tuple',[],[{'Atom',[],['stop']}]}]}";
		OtpErlangObject obj = ErlangLabel.parseText("{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Atom',[],['tmp']}]},"+nestedTerm+"]}");
		final Signature sig = Signature.buildFromType(obj);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		}},IllegalArgumentException.class,"tuple too short");
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue6()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'None',[]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'None',[]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue7()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Any',[]}");
		Signature sig = Signature.buildFromType(obj);
		Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Any',[]}",conversionResult.toErlangTerm());
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue8()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Atom',[],['tmp']}]},"+
				"{'Atom',[],['normal']}"+
				"]}");
		final Signature sig = Signature.buildFromType(obj);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		}},IllegalArgumentException.class,"expected a tuple or an alt");
	}
	
	/** Tests the extraction of return values. */
	@Test
	public void testExtractionOfOTPreturnValue9()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'Alt',[],[{'Tuple',[],[{'Atom',[],['noreply']},{'Atom',[],['tmp']}]},"+
				"{'Any',[]}"+
				"]}");
		Signature sig = Signature.buildFromType(obj);
		final Signature conversionResult = new OTPBehaviour.OTPCall().extractVisibleReturnType(sig);
		Assert.assertEquals("{'Alt',[],[{'Atom',[],['tmp']},{'Any',[]}]}",conversionResult.toErlangTerm());
	}
	
	/** Checks that an exception is thrown if the return value is not compliant with what expect OTP to produce. */
	@Test
	public void testTypeCompatibilityInvalidReturnValue1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n"+
		"\nhandle_call(45,_,_)->ok.\nhandle_cast(_,_)->ok.\n\ninit(_)->ok.\nhandle_info(_,_)->ok.\n");wr.close();
		
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(new File(erlangFile));
		}},IllegalArgumentException.class,"expected a tuple or an alt");
	}
	
	/** Checks that an exception is thrown if the return value is not compliant with what expect OTP to produce. */
	@Test
	public void testTypeCompatibilityInvalidReturnValue2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n"+
		"\nhandle_call(45,_,_)->{reply}.\nhandle_cast(_,_)->{reply,ok}.\n\ninit(_)->ok.\nhandle_info(_,_)->{reply,ok}.\n");wr.close();
		
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(new File(erlangFile));
		}},IllegalArgumentException.class,"tuple too short");
	}
	
	protected static final String otherMethods = "\nhandle_cast(_,_)->{noreply,5}.\n\ninit(_)->{ok,5}.\nhandle_info(_,_)->{noreply,5}. ";
	
	/** Gives an integer with values falling in a specific range. */
	@Test
	public void testTypeCompatibility1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n"+
		"\nhandle_call(45,_,_)->{reply,ok,5}.\n");wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 45, ok",config);
		checkFailureFor("call, [45], ok", config);
		checkFailureFor("call, {45}, ok", config);
		checkFailureFor("call, aa, ok", config);
		checkFailureFor("call, 45, oak", config);
		checkFailureFor("call, 46, ok", config);
	}

	/** Gives an arbitrary integer. */
	@Test
	public void testTypeCompatibility2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		Random rnd = new Random(0);
		Set<Integer> num = new HashSet<Integer>();
		for(int i=0;i< 30;++i)
			if (!num.contains(i))
			{
				num.add(i);
				wr.write("handle_call("+rnd.nextInt()+",_,_)->{reply,ok,5};\n");
			}
		int n = rnd.nextInt();
		while(num.contains(n))
			n = rnd.nextInt();
		wr.write("handle_call("+n+",_,_)->{reply,ok,5}.\n");
		
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 45, ok",config);
		checkFailureFor("call, [45], ok", config);
		checkFailureFor("call, {45}, ok", config);
		checkFailureFor("call, aa, ok", config);
		checkFailureFor("call, 45, oak", config);
		createLabel("call, 46, ok", config);
	}

	@Test
	public void testTypeCompatibility3() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		for(int i=1;i< 30;++i)
				wr.write("handle_call("+i+",_,_)->{reply,ok,5};\n");
			
		wr.write("handle_call("+56+",_,_)->{reply,ok,5}.\n");
		
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 98, ok",config);
		checkFailureFor("call, [45], ok", config);
		checkFailureFor("call, {45}, ok", config);
		checkFailureFor("call, aa, ok", config);
		checkFailureFor("call, 256, ok", config);
		createLabel("call, 46, ok", config);
	}

	@Test
	public void testNumberCompatibilityPositive() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call("+1+",_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
/*		OtpErlangObject funcDescr = ErlangLabel.parseText(mod.sigs.get("call").toErlangTerm().replace("boundaries", "positive"));
		System.out.println(funcDescr);*/
		mod.sigs.put("funcPositive", new FuncSignature(ErlangLabel.parseText("{\"testFile.erl\",3,handle_call,1,"+
		"{'Func',[],[{'Int',[positive]}],{'Atom',[],[ok]}}}"),null));
		
		createLabel("funcPositive, 998, ok",config);
		checkFailureFor("funcPositive, [45], ok", config);
		checkFailureFor("funcPositive, 0, ok", config);
		checkFailureFor("funcPositive, -20, ok", config);
	}

	@Test
	public void testNumberCompatibilityNegative() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call("+1+",_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		mod.sigs.put("funcNegative", new FuncSignature(ErlangLabel.parseText("{\"testFile.erl\",3,handle_call,1,"+
		"{'Func',[],[{'Int',[negative]}],{'Atom',[],[ok]}}}"),null));
		
		checkFailureFor("funcNegative, 998, ok",config);
		checkFailureFor("funcNegative, [45], ok", config);
		checkFailureFor("funcNegative, 0, ok", config);
		createLabel("funcNegative, -20, ok", config);
	}

	@Test
	public void testNumberCompatibilityNonnegative() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call("+1+",_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		mod.sigs.put("funcNonnegative", new FuncSignature(ErlangLabel.parseText("{\"testFile.erl\",3,handle_call,1,"+
		"{'Func',[],[{'Int',[nonnegative]}],{'Atom',[],[ok]}}}"),null));
		
		createLabel("funcNonnegative, 998, ok",config);
		checkFailureFor("funcNonnegative, [45], ok", config);
		createLabel("funcNonnegative, 0, ok", config);
		checkFailureFor("funcNonnegative, -20, ok", config);
	}

	@Test
	public void testCompatibilityListNonEmptyPossiblyImproper()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'List',['nonempty','maybeimproper'],[]}");
		final Signature sig = Signature.buildFromType(obj);
		Assert.assertFalse(sig.typeCompatible(ErlangLabel.parseText("[]")));
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[5]")));
		Assert.assertTrue(sig.typeCompatible(ErlangRunner.getRunner().evaluateString("[2|3]")));
	}
	
	@Test
	public void testCompatibilityListNonEmpty()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'List',['nonempty'],[]}");
		final Signature sig = Signature.buildFromType(obj);
		Assert.assertFalse(sig.typeCompatible(ErlangLabel.parseText("[]")));
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[5]")));
		Assert.assertFalse(sig.typeCompatible(ErlangRunner.getRunner().evaluateString("[2|3]")));
	}
	
	@Test
	public void testCompatibilityListNonEmptyImproper()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'List',['nonempty','improper'],[]}");
		final Signature sig = Signature.buildFromType(obj);
		Assert.assertFalse(sig.typeCompatible(ErlangLabel.parseText("[]")));
		Assert.assertFalse(sig.typeCompatible(ErlangLabel.parseText("[5]")));
		Assert.assertTrue(sig.typeCompatible(ErlangRunner.getRunner().evaluateString("[2|3]")));
	}
		
	@Test
	public void testCompatibilityListA()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'List',[],[]}");
		final Signature sig = Signature.buildFromType(obj);
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[]")));
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[5]")));
		Assert.assertFalse(sig.typeCompatible(ErlangRunner.getRunner().evaluateString("[2|3]")));
	}
	
	@Test
	public void testCompatibilityListB()
	{
		OtpErlangObject obj = ErlangLabel.parseText("{'List',[],[{'Atom',[],[aa,bb]}]}");
		final Signature sig = Signature.buildFromType(obj);
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[]")));
		Assert.assertTrue(sig.typeCompatible(ErlangLabel.parseText("[bb]")));
		Assert.assertFalse(sig.typeCompatible(ErlangLabel.parseText("[5]")));
		Assert.assertFalse(sig.typeCompatible(ErlangRunner.getRunner().evaluateString("[2|3]")));
	}
	
	@Test
	public void testCompatibilityList1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call([67,1900,atom],_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 256, ok",config);
		createLabel("call, [67], ok",config);
		createLabel("call, [atom,67,67,1900], ok",config);
		checkFailureFor("call, 67, ok", config);
		createLabel("call, [], ok", config);// possibly empty
		checkFailureFor("call, [67,256], ok", config);
		checkFailureFor("call, 256, 67", config);
	}
	
	/** Pretty-prints the alphabet of the module supplied. */
	public static String getAlphabetAsString(ErlangModule mod)
	{
		return RPNILearner.questionToString(Arrays.asList(mod.behaviour.getAlphabet().toArray(new Label[0])));		
	}
	/*
	private static void dumpFuncDescr(ErlangModule mod)
	{
		OtpErlangObject funcDescr = ErlangLabel.parseText(mod.sigs.get("call").toErlangTerm().replace("boundaries", "positive"));
		System.out.println(funcDescr);
		System.out.println("Alphabet: "+getAlphabetAsString(mod));
	}
	*/
	/** Checks for improper lists. */
	@Test
	public void testCompatibilityList2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call([67,1900,atom|junk],_,_)->{reply,ok,5};\n");
		wr.write("handle_call([\"str\"|55],_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 256, ok",config);
		checkFailureFor("call, [\"rrr\"], ok",config);
		checkFailureFor("call, [atom,67,67,1900], ok",config); // proper list
		checkFailureFor("call, 67, ok", config);
		checkFailureFor("call, [], ok", config);// non-empty list
		checkFailureFor("call, [67,256], ok", config);
		checkFailureFor("call, 256, 67", config);
		
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom,67,67,1900| 55],ok}"),config);
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"rrr\",67,67,1900| junk],ok}"),config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom,67,67,1900| a],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"u\",67,67,1900| junk],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"ru\",67,67,1900| junk],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
	}
	
	/** Checks for improper lists. */
	@Test
	public void testCompatibilityList3a() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call([67,1900,atom|junk],_,_)->{reply,ok,5};\n");
		wr.write("handle_call([\"str\"],_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);

		createLabel("call, 256, ok",config);
		createLabel("call, [\"rrr\"], ok",config);
		createLabel("call, [atom,67,67,1900], ok",config); // proper list
		checkFailureFor("call, 67, ok", config);
		checkFailureFor("call, [], ok", config);// non-empty list
		checkFailureFor("call, [67,256], ok", config);
		checkFailureFor("call, 256, 67", config);
		
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom,67,67,1900| junk],ok}"),config);
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"rrr\",67,67,1900| junk],ok}"),config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom,67,67,1900| a],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"u\",67,67,1900| junk],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[\"ru\",67,67,1900| junk],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
	}
	
	/** Checks for properly improper lists. */
	@Test
	public void testCompatibilityList3b() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("handle_call([67,1900,atom|junk],_,_)->{reply,ok,5};\n");
		wr.write("handle_call("+256+",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, 256, ok",config);
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call, [atom|junk], ok}"),config);
		ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call, [atom |junk], ok}"),config);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom,67,67,1900],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[atom|a],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangLabel.erlangObjectToLabel(ErlangRunner.getRunner().evaluateString("{call,[ato|junk],ok}"),config);
		}},IllegalArgumentException.class,"not type-compatible");
	}
	
	@Test
	public void testCheckEmptyString1()
	{
		Assert.assertFalse(ListSignature.checkEmptyList(new OtpErlangAtom("")));
		Assert.assertTrue(ListSignature.checkEmptyList(new OtpErlangList()));
		Assert.assertTrue(ListSignature.checkEmptyList(new OtpErlangString("")));
	}
	
	@Test
	public void testCheckEmptyStringFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ListSignature.checkEmptyList(new OtpErlangString("a"));
		}},IllegalArgumentException.class,"the last tail of an improper list");
	}
	
	@Test
	public void testCheckEmptyStringFail2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ListSignature.checkEmptyList(new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("a")}));
		}},IllegalArgumentException.class,"the last tail of an improper list");
	}
	
	/** Tests for strings. */
	@Test
	public void testCompatibilityList4() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(\"this is a test\",_,_)->{reply,ok,5};\n");
		wr.write("handle_call(\"more interesting things\",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		createLabel("call, \"ttt\",ok",config);
		createLabel("call, \"ttt\"",config);
		checkFailureFor("call, \"ttt\", \"ttt\"",config);
		checkFailureFor("call, 256, ok",config);
	}
	
	/** Tests for tuples and strings. */
	@Test
	public void testCompatibilityList5() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call({\"this is a test\",\"aa\"},_,_)->{reply,\"yy\",5};\n");
		wr.write("handle_call(\"more interesting things\",_,_)->{reply,ok,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		createLabel("call, \"ttt\",ok",config);
		createLabel("call, \"ttt\",\"y\"",config);
		createLabel("call, {\"ttt\",\"a\"},\"y\"",config);
		createLabel("call, {\"\",\"\"},\"y\"",config);
		createLabel("call, {\"\",\"\"},\"\"",config);
		createLabel("call, \"ttt\"",config);
		checkFailureFor("call, \"ttt\", \"ttt\"",config);
		checkFailureFor("call, 256, ok",config);
		checkFailureFor("call,  {\"ttt\",\"b\"}, ok",config);
		checkFailureFor("call,  {[90],\"b\"}, ok",config);
		checkFailureFor("call,  {\"ttt\",[-3]}, ok",config);
		
		// now force the function to require non-empty tails
		//dumpFuncDescr(mod);
		mod.sigs.put("func", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'Alt',[],[{'List',[nonempty],[{'Int',[values],\" eghimnorst\"}]},{'Tuple',[],[{'List',[],[{'Int',[values],\" aehist\"}]},{'List',[],[{'Int',[values],\"a\"}]}]}]}],{'Alt',[],[{'Atom',[],[ok]},{'List',[],[{'Int',[values],\"y\"}]}]}}}"
		),null));
		createLabel("func, {\"ttt\",\"a\"},\"y\"",config);
		createLabel("func, {\"ttt\",\"\"},\"\"",config);
		checkFailureFor("func, \"\",\"\"",config);// first list is empty
		checkFailureFor("func, {\"\",\"y\"},\"\"",config);
		checkFailureFor("func, {\"g\",\"\"},\"\"",config);
		checkFailureFor("func, {\"g\",\"\"},\"t\"",config);
	}
	
	/** Tests for tuples and strings. */
	@Test
	public void testTypeCompatibility4() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == {4,f} -> {reply,wr(F),5}.\n");
		wr.write("wr({A,B}) -> A+1.");
		//wr.write("handle_call(F,_,_) when F > 0 ->\"yy\".\n");
		//wr.write("handle_call(F,_,_) when F == self() ->\"yy\".\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, {33,\"a\"},45",config);
		createLabel("call, {33,\"a\"},245",config);
		checkFailureFor("call, {33,\"a\"},a245",config);
		checkFailureFor("call, {a33,\"a\"},245",config);
	}
	
	/** Tests for tuples and strings. */
	@Test
	public void testTypeCompatibility5() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == {} -> {reply,wr(F),5}.\n");
		wr.write("wr({}) -> 1.");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		createLabel("call, {},1",config);
		checkFailureFor("call, {44},1",config);
		checkFailureFor("call, {33,\"a\"},a245",config);
		checkFailureFor("call, {a33,\"a\"},245",config);
	}
	
	/** Tests for an empty list. */
	@Test
	public void testTypeCompatibility6() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == [] -> {reply,wr(F),5}.\n");
		wr.write("wr([]) -> 1.");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		createLabel("call, [],1",config);
		checkFailureFor("call, [44],1",config);
		checkFailureFor("call, [33,\"a\"],a245",config);
		checkFailureFor("call, [a33,\"a\"],245",config);
	}
	
	/** Tests for strings. */
	@Test
	public void testTypeCompatibility7() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == [] -> {reply,wr(F),5}.\n");
		wr.write("wr([]) -> 1.");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		mod.sigs.put("strA", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[],[[],\"strA\"]}],{'Int',[values],[1]}}}"),
		null));
		mod.sigs.put("strB", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[],[\"strB\"]}],{'Int',[values],[1]}}}"),
		null));
		mod.sigs.put("strC", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[]}],{'Int',[values],[1]}}}"),
		null));
		mod.sigs.put("strCnonEmpty", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[nonempty]}],{'Int',[values],[1]}}}"),
		null));
		
		createLabel("strA, [],1",config);
		createLabel("strA, \"strA\",1",config);
		checkFailureFor("strA, \"strB\",1",config);
		createLabel("strB, \"strB\",1",config);
		checkFailureFor("strB, [],1",config);
		checkFailureFor("strB, \"strA\",1",config);
		checkFailureFor("strA, [44],1",config);

		createLabel("strC, [],1",config);
		createLabel("strC, \"strA\",1",config);
	
		checkFailureFor("strCnonEmpty, [],1",config);
		createLabel("strCnonEmpty, \"strA\",1",config);
	}

	/** Tests for an arbitrary list. */
	@Test
	public void testTypeCompatibility8() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == [] -> {reply,wr(F),5}.\n");
		wr.write("wr([]) -> 1.");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		mod.sigs.put("strNonEmpty", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[],[[]]}],{'Int',[values],[1]}}}"),
		null));
		mod.sigs.put("strNonEmpty", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'String',[],[[]]}],{'Int',[values],[1]}}}"),
		null));
		
		createLabel("call, [],1",config);
		checkFailureFor("call, [44],1",config);
		checkFailureFor("call, [33,\"a\"],a245",config);
		checkFailureFor("call, [a33,\"a\"],245",config);
	}
	
	/** Tests for floating-point numbers. */
	@Test
	public void testTypeCompatibilityFloat() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(4.8,_,_) -> {reply,1,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		createLabel("call, 4.8,1",config);
		createLabel("call, 4.7,1",config);
		checkFailureFor("call, 4,1",config);
		checkFailureFor("call, 4.8,2",config);
		checkFailureFor("call, 4.8,1.1",config);
		checkFailureFor("call, [],1",config);
		checkFailureFor("call, [33,\"a\"],a245",config);
		checkFailureFor("call, [a33,\"a\"],245",config);
	}
	
	/** Tests for bit strings numbers - for an unknown reason, the return type turns out to be ?none. */
	@Test
	public void testTypeCompatibilityBitString1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(<< 4:2,5:6 >>,_,_) -> {reply,1,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		checkFailureFor("call, 4,1",config);
	}
	
	/** Tests for bit strings numbers - the return type of bitstring turns out to be ?none. */
	@Test
	public void testTypeCompatibilityBitString2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(_,_,_) -> {reply,<< 4:2,5:6 >>,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		Assert.assertEquals("[{?F(),'call','AnyWibble',<< 34, 56>>},{?F(),'cast','AnyWibble',5},{?F(),'info','AnyWibble',{'noreply',5}},{?F(),'init','AnyWibble','ok'}]",getAlphabetAsString(mod));
		createLabel("call, 4.7,<< 45>> ",config);
		checkFailureFor("call, 4,1",config);
	}
	
	/** Tests that construction of an instance of the ?none produces nothing. */
	@Test
	public void testTypeNone() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(F,_,_) when F == {4,f} -> {reply,wr(F),5}.\n");
		wr.write("wr(A) -> A+1.");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		Assert.assertEquals("[{?F(),'cast','AnyWibble',5},{?F(),'info','AnyWibble',{'noreply',5}},{?F(),'init','AnyWibble','ok'}]",getAlphabetAsString(mod));
	}
	
	/** Tests for bit strings numbers - for an unknown reason, the return type turns out to be ?none. */
	@Test
	public void testTypeCompatibilityBoolean1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call(true,_,_) -> {reply,1,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		
		createLabel("call, true,1",config);
		checkFailureFor("call, afalse,1",config);
		checkFailureFor("call, false,1",config);
	}
	
	/** Tests for bit strings numbers - for an unknown reason, the return type turns out to be ?none. */
	@Test
	public void testTypeCompatibilityBoolean2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");

		wr.write("handle_call([false,true],_,_) -> {reply,1,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);
		mod.sigs.put("bool", new FuncSignature(ErlangLabel.parseText(
		"{\"testFile.erl\",3,handle_call,1,{'Func',[],[{'Boolean',[]}],{'Int',[values],[1]}}}"),
		null));
		
		createLabel("bool, true,1",config);
		createLabel("bool, false,1",config);
		checkFailureFor("bool, afalse,1",config);
		checkFailureFor("bool, 56,1",config);
		checkFailureFor("bool, [true],1",config);
		
		createLabel("call, [true],1",config);
		createLabel("call,[ false],1",config);
		checkFailureFor("call, [afalse],1",config);
		checkFailureFor("call, afalse,1",config);
		checkFailureFor("call, 56,1",config);
	}
	
	@Test
	public void testTypeCompatibilityVariedSignaturesFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getRunner().evaluateString("typer_annotator_s:t_to_Statechum(erl_types:t_from_term(fun (A) -> A+1 end),dict:new())");
		}},RuntimeException.class,"Unsupported type functions");
	}
	
	@Test
	public void testTypeCompatibilityPid()
	{
		Signature sig = Signature.buildFromType(ErlangRunner.getRunner().evaluateString("typer_annotator_s:t_to_Statechum(erl_types:t_from_term(self()),dict:new())"));
		Assert.assertFalse(sig.instantiateAllAlts().isEmpty());
		Assert.assertTrue(sig.instantiateAllAlts().iterator().next() instanceof OtpErlangPid);
		
	}
	@Test
	public void testTypeCompatibilityPort()
	{
		Signature sig = Signature.buildFromType(ErlangRunner.getRunner().evaluateString("[P|_]=erlang:ports(),typer_annotator_s:t_to_Statechum(erl_types:t_from_term(P),dict:new())"));
		Assert.assertFalse(sig.instantiateAllAlts().isEmpty());
		Assert.assertTrue(sig.instantiateAllAlts().iterator().next() instanceof OtpErlangPort);
	}
	
	/** Non-intersecting values of records give rise to "any" - type. */
	@Test
	public void testTypeCompatibilityRecords1() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("-record(st, {processNum,smth=\"junk\",compiledModules=sets:new()}).\n");
		wr.write("handle_call(#st{processNum=33}=Arg,_,_) -> {reply,11,5};\n");
		wr.write("handle_call(#st{smth=\"whatever\"}=Arg,_,_) -> {reply,12,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);

		checkFailureFor("call, afalse,11",config);
		Assert.assertEquals("[{?F(),'call',{'st'},11},{?F(),'cast','AnyWibble',5},{?F(),'info','AnyWibble',{'noreply',5}},{?F(),'init','AnyWibble','ok'}]",
				getAlphabetAsString(mod));
	}
	
	@Test
	public void testTypeCompatibilityRecords2() throws IOException
	{
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n");
		wr.write("-record(st, {processNum,smth=\"junk\",compiledModules=sets:new()}).\n");
		wr.write("handle_call(#st{processNum=33,smth=\"whatever\"}=Arg,_,_) -> {reply,11,5};\n");
		wr.write("handle_call(#st{processNum=11,smth=\"Awhatever\"}=Arg,_,_) -> {reply,12,5}.\n");
		wr.write(otherMethods);wr.close();
		ErlangModule mod = ErlangModule.loadModule(new File(erlangFile));
		final Configuration config = Configuration.getDefaultConfiguration().copy();config.setErlangModuleName(mod.getName());config.setLabelKind(LABELKIND.LABEL_ERLANG);

		checkFailureFor("call, afalse,11",config);
		Assert.assertEquals("[{?F(),'call',{'st',11,[]},11},{?F(),'call',{'st',11,[65,65]},11},{?F(),'cast','AnyWibble',5},{?F(),'info','AnyWibble',{'noreply',5}},{?F(),'init','AnyWibble','ok'}]", 
				getAlphabetAsString(mod));
	}
}
