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
package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Random;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.GlobalConfiguration;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.ProgressIndicator;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class TestErlangRunner {

	ErlangRuntime erlRuntime = null;
	
	/** This one is used to number work directories so that different tests do not affect each other. Unfortunately, the numbering is sequential hence it is not known which test corresponds to which number. */
	protected static int number = 0;

	/** URL of the writable directory to be used for tests. */
	public File testDir = null;
	
	/** URL of the locker example. */
	public final File erlangLocker = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES),"locker"+File.separator+"locker.erl");
	
	protected ErlangRunner runner;
	
	File output = null;
	
	@Before
	public void beforeTest()
	{
		erlRuntime = new ErlangRuntime();erlRuntime.setTimeout(500);erlRuntime.startRunner();
		runner = erlRuntime.createNewRunner();
		testDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP),"__TestErlangRunner__"+(number++));
		output = new File(testDir,"test.erl");
		createTestDir();
	}
	
	public void createTestDir()
	{
		if (!testDir.isDirectory())
		{
			Assert.assertTrue("could not create "+testDir.getAbsolutePath(),testDir.mkdir());
		}
	}

	@After
	public void afterTest()
	{
		if (runner != null) runner.close();
		erlRuntime.killErlang();
		zapTestDir();
	}
	
	public void zapTestDir()
	{
		ExperimentRunner.zapDir(testDir);
	}
	
	@Test
	public void testGetErlName1()
	{
		checkValid("aa","aa.erl");
		checkValid("aa","  aa.erl   ");
	}

	@Test
	public void testGetErlName2()
	{
		checkValid("aa/bb","  aa/bb.erl   ");
		checkValid("aa/bb...","  aa/bb....erl   ");
		checkValid("aa/bb.c.d","  aa/bb.c.d.erl   ");
		checkValid("aa/bb.c.d.erl","  aa/bb.c.d.erl.erl   ");
	}
	
	void checkValid(String expected, String whatToPassToGetErlName)
	{
		Assert.assertEquals(expected,ErlangRunner.getErlName(whatToPassToGetErlName));Assert.assertTrue(ErlangRunner.validName(whatToPassToGetErlName));
	}
	
	void checkInvalid(String name)
	{
		Assert.assertNull(ErlangRunner.getErlName(name));Assert.assertFalse(ErlangRunner.validName(name));
	}
	
	@Test
	public void testGetErlNameFail()
	{
		checkInvalid(".erl");
		checkInvalid(null);
		checkInvalid("aa.berl");
		checkInvalid("aa.berl");
		checkInvalid("aa.erl.tt");
		checkInvalid("aa");
	}
	
	@Test
	public void testGetName1()
	{
		Assert.assertTrue(ErlangRunner.getName(new File("bb.c.d.erl.erl"),ERL.BEAM, true).endsWith("bb.c.d.erl.beam"));
		Assert.assertTrue(ErlangRunner.getName(new File("  bb....erl   "),ERL.BEAM, true).endsWith("bb....beam"));
	}
	
	@Test
	public void testGetNameFail()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			ErlangRunner.getName(new File("  aa/bb....   "),ERL.BEAM, true);
		}},IllegalArgumentException.class,"Invalid module");

	}
	
	/** Tests the echo method of the server, i.e.
	 * { dataHead, ['Data2',data3 ] } = gen_server:call(echo,[ dataHead, 'Data2', data3 ]) 
	 */
	@Test
	public void testErlangRunner1()
	{
		checkEcho(erlRuntime.createNewRunner());
	}
	
	/** Tests the echo method of the server, i.e.
	 * { dataHead, ['Data2',data3 ] } = gen_server:call(echo,[ dataHead, 'Data2', data3 ]) 
	 * repeated a few times.
	 */
	@Test
	public void testErlangRunner2()
	{
		for(int i=0;i< 100;++i) checkEcho(erlRuntime.createNewRunner());
	}
	
	/** Tests multiple instances of erlang. 
	 * @throws OtpErlangRangeException */
	@Test
	public void testErlangRunner3_longrunning_28sec() throws OtpErlangRangeException
	{
		GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.ASSERT_ENABLED);// force configuration load and all related messages so that they do not interfere with progress indicator.
		ErlangRuntime []runners = new ErlangRuntime[7];// more could be not allowed by XP Pro :/
		String processNames[] = new String[runners.length];
		int currentNumber =0;
		for(int i=0;i<runners.length;++i) 
		{ 
			runners[i]=new ErlangRuntime();processNames[i]=""+currentNumber++;runners[i].startErlang(processNames[i], 0); 
		}
		Random rnd = new Random(0);
		final int testNumber = 1000;
		ProgressIndicator progress = new ProgressIndicator("Random erlang tests", testNumber);
		for(int cnt=0;cnt < testNumber;++cnt)
		{
			int runnerNumber = rnd.nextInt(runners.length);
			ErlangRuntime r = runners[runnerNumber];
			if (rnd.nextInt(100) > 1)
			{
				int numA = rnd.nextInt(10000)-5000, numB=rnd.nextInt(10000)-5000;
				ErlangRunner rn = new ErlangRunner(r.traceRunnerNode);rn.forceReady();// only using default instances
				OtpErlangTuple response = (OtpErlangTuple)rn.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
						new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead), new OtpErlangInt(numA), new OtpErlangInt(numB),new OtpErlangAtom(dataC)})},
						0);

				Assert.assertEquals(dataHead,((OtpErlangAtom)response.elementAt(0)).atomValue());
				Assert.assertEquals(processNames[runnerNumber],((OtpErlangAtom)response.elementAt(1)).atomValue());
				OtpErlangObject [] list = ((OtpErlangList)response.elementAt(2)).elements();
				Assert.assertEquals(3, list.length);
				Assert.assertEquals(numA,((OtpErlangLong)list[0]).intValue());
				Assert.assertEquals(numB,((OtpErlangLong)list[1]).intValue());
				Assert.assertEquals(dataC,((OtpErlangAtom)list[2]).atomValue());
			}
			else 
			{
				r.killErlang();processNames[runnerNumber]=""+currentNumber++;runners[runnerNumber].startErlang(processNames[runnerNumber], 0);
			}
			progress.next();
		}
		for(int i=0;i<runners.length;++i) runners[i].killErlang();
		Assert.assertTrue(currentNumber > runners.length+10);
	}
	
	
	protected static final String dataHead = "dataHead", dataB = "Data2", dataC = "data3";

	protected void checkEcho(ErlangRunner erlang)
	{
		OtpErlangTuple tuple = makeCall(erlang);
		Assert.assertEquals(dataHead,((OtpErlangAtom)tuple.elementAt(0)).atomValue());
		Assert.assertTrue( ((OtpErlangAtom)tuple.elementAt(1)).atomValue().startsWith("erlangRunner_"));
		OtpErlangObject [] list = ((OtpErlangList)tuple.elementAt(2)).elements();
		Assert.assertEquals(2, list.length);
		Assert.assertEquals(dataB,((OtpErlangAtom)list[0]).atomValue());
		Assert.assertEquals(dataC,((OtpErlangAtom)list[1]).atomValue());
	}

	protected OtpErlangTuple makeCall(ErlangRunner erlang)
	{
		return (OtpErlangTuple)erlang.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
				new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead), new OtpErlangAtom(dataB), new OtpErlangAtom(dataC)})}
				,2000);
	}
	
	@Test
	public void testCompileFailure0a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(new File("junk"),null,true);
		}},IllegalArgumentException.class,"Invalid module");
	}
	
	@Test
	public void testCompileFailure1a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(new File("junk.erl"),null,true);
		}},IllegalArgumentException.class,"does not have a parent directory");
	}
	
	public static final String someErlang = "-module(test).\n-export([testFun/1]).\ntestFun([Arg])->io:format(\"42~n\"),halt().\n";
	
	@Test
	public void testCompileFailure2a() throws IOException
	{
		Writer wr = new FileWriter(output);wr.write(someErlang);wr.write("someJunk");wr.close();
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(output,null,true);
		}},IllegalArgumentException.class,"Failure running erlc");
	}
	
	@Test
	public void testCompileFailure2b() throws IOException
	{
		Writer wr = new FileWriter(output);wr.write(someErlang);wr.write("someJunk");wr.close();
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(output,erlRuntime.createNewRunner(),true);
		}},IllegalArgumentException.class,"failedToCompile");// error message returned by Erlang code
	}
	
	@Test
	public void testCompile1a() throws IOException
	{
		ErlangRunner.compileErl(new File(ErlangRunner.getErlangFolder(),"tracerunner.erl"),null,true);
	}
	
	@Test
	public void testCompile1b() throws IOException
	{
		ErlangRunner.compileErl(new File(ErlangRunner.getErlangFolder(),"tracerunner.erl"),erlRuntime.createNewRunner(),true);
	}
	
	public void createAndCompile(String MagicNumber,ErlangRunner runnerToUse) throws IOException
	{
		Writer wr = new FileWriter(output);wr.write(someErlang.replace("42", MagicNumber));wr.close();
		ErlangRunner.compileErl(output,runnerToUse,false);
		
	}
	public void attemptRun(String MagicNumber) throws IOException
	{
        Process erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl","-run",ErlangRunner.getErlName(output.getName()),"testFun","arg",
        		"-noshell"}, null, testDir);
    	final StringBuffer err=new StringBuffer(),out=new StringBuffer(); 
        ExperimentRunner.dumpStreams(erlangProcess, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(StringBuffer b) {
                err.append(b);
            }

            @Override
            public void StdOut(StringBuffer b) {
            	out.append(b);
            }
        });
        try {
        	erlangProcess.waitFor();
        } catch (InterruptedException e) {
            
        }
        if (erlangProcess.exitValue() != 0)
        	throw new IllegalArgumentException("Failure running "+output.getName()+"\n"+err+(err.length()>0?"\n":"")+out);
        Assert.assertEquals(MagicNumber+"\n",out.toString());
        Assert.assertEquals("Expected empty error stream, got "+err.toString(),0,err.length());
	}

	@Test
	public void testCompileAndRun1a() throws IOException
	{
		createAndCompile("42",null);attemptRun("42");
	}

	@Test
	public void testCompileAndRun1b() throws IOException
	{
		createAndCompile("42",erlRuntime.createNewRunner());attemptRun("42");
	}

	public void checkCompileHonoursModifyDate() throws IOException, InterruptedException
	{
		createAndCompile("42",runner);attemptRun("42");
		File
			origName = new File(ErlangRunner.getName(output,ERL.BEAM,false)),
			tmpName = new File(ErlangRunner.getName(output,ERL.PLT,false));
		
		origName.renameTo(tmpName);
		Thread.sleep(2000);
		createAndCompile("43",runner);
		Assert.assertTrue(origName.delete());tmpName.renameTo(origName);// restore the original .beam
		attemptRun("42");// verify rename worked - running original beam.
		ErlangRunner.compileErl(output,runner,false);// compile, beam should be replaced
		attemptRun("43");// verify it is now the new .beam
		origName.renameTo(tmpName);// now make a copy of it
		createAndCompile("44",runner);
		Assert.assertTrue(origName.delete());tmpName.renameTo(origName);// restore the '43 .beam
		Assert.assertTrue(origName.setLastModified(output.lastModified()+100000L));
		ErlangRunner.compileErl(output,runner,false);// compile, beam should be preserved
		attemptRun("43");// verify it is the preserved .beam
	}
	
	/** Tests the if a source is modified, it will be recompiled. 
	 * @throws InterruptedException */
	@Test
	public void testCompileAndRun2a() throws IOException, InterruptedException
	{
		checkCompileHonoursModifyDate();
	}
	
	/** Tests the if a source is modified, it will be recompiled. 
	 * @throws InterruptedException */
	@Test
	public void testCompileAndRun2b() throws IOException, InterruptedException
	{
		checkCompileHonoursModifyDate();
	}
	
	@Test
	public void testCompileAndRunFailure() throws IOException
	{
		Writer wr = new FileWriter(output);wr.write(someErlang);wr.close();
		ErlangRunner.compileErl(output,null,false);
        Process erlangProcess = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin() + "erl","-run",ErlangRunner.getErlName(output.getName()),"testFun","arg","arg2",
        		"-noshell"}, null, testDir);
    	final StringBuffer err=new StringBuffer(),out=new StringBuffer(); 
        ExperimentRunner.dumpStreams(erlangProcess, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(StringBuffer b) {
                err.append(b);
            }

            @Override
            public void StdOut(StringBuffer b) {
            	out.append(b);
            }
        });
        try {
        	erlangProcess.waitFor();
        } catch (InterruptedException e) {
        }
        Assert.assertEquals(1,erlangProcess.exitValue());
        Assert.assertTrue(out.toString().contains("function_clause"));
        // When this test runs alongside others in TestErlangRunner, the error message
        // starting with "Crash dump was written to: erl_crash.dump" is written into StdErr,
        // but when I run it separately, it goes into StdOut. This most likely relates to a
        // problem with how Java 1.7.10 does things so I work around but permitting both 
        // responses.
        String msgWithErrorText = null;
        if (err.length() > 5) // an arbitrary number to account for a possible end of line.
        	msgWithErrorText = err.toString();
        else
        	msgWithErrorText = out.toString();
        Assert.assertTrue("Unexpected error message: "+msgWithErrorText,
        		msgWithErrorText.contains("Crash dump was written to: erl_crash.dump") || msgWithErrorText.contains("Crash dump is being written to: erl_crash.dump"));
	}

	protected boolean registeredProcessExists(ErlangRunner r)
	{
		String response = (runner.evaluateString("case whereis("+r.getRunnerName()+") of undefined -> false;Pid when is_pid(Pid) -> true end")).toString();
		if (response.equals("true"))// here we are checking specific values because this is what is to be returned from the Erlang expression above.
			return true;
		if (response.equals("false"))
			return false;
		throw new IllegalArgumentException("invalid response from Erlang, received "+response);
	}
	
	/** Tests that an attempt to register a runner with the same ID fails. */
	@Test
	public void testRunnerInitialisationFailure()
	{
		Assert.assertTrue(registeredProcessExists(runner));

		final ErlangRunner r = new ErlangRunner(erlRuntime.traceRunnerNode);
		Assert.assertFalse(r.mboxOpen);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			r.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"is closed");
		Assert.assertFalse(registeredProcessExists(r));

		r.initRunner();
		Assert.assertTrue(r.mboxOpen);
	
		OtpErlangTuple response = (OtpErlangTuple)r.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
				new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead)})},
				0);
		Assert.assertEquals(dataHead,((OtpErlangAtom)response.elementAt(0)).atomValue());

		Assert.assertTrue(registeredProcessExists(r));

		checkForCorrectException(new whatToRun() { public @Override void run() {
			r.initRunner();// this will fail because the corresponding process is already running.
		}},IllegalArgumentException.class,"already_started");
		Assert.assertFalse(r.mboxOpen);
		Assert.assertFalse(registeredProcessExists(r));
		
		// verify failure
		checkForCorrectException(new whatToRun() { public @Override void run() {
			r.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"is closed");
		
	}
	
	@Test
	public void testClosedRunner1()
	{
		Assert.assertTrue(registeredProcessExists(runner));

		final ErlangRunner r = new ErlangRunner(erlRuntime.traceRunnerNode);
		Assert.assertFalse(r.mboxOpen);
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			r.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"is closed");
		Assert.assertFalse(registeredProcessExists(r));
	}

	/** Tests that an attempt to use a closed runner ID fails. */
	@Test
	public void testClosedRunner2()
	{
		final ErlangRunner r = new ErlangRunner(erlRuntime.traceRunnerNode);
		Assert.assertFalse(r.mboxOpen);
		r.initRunner();
		Assert.assertTrue(r.mboxOpen);
		
		OtpErlangTuple response = (OtpErlangTuple)r.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
				new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead)})},
				0);
		Assert.assertEquals(dataHead,((OtpErlangAtom)response.elementAt(0)).atomValue());

		Assert.assertTrue(registeredProcessExists(r));

		r.close();
		Assert.assertFalse(r.mboxOpen);
		Assert.assertFalse(registeredProcessExists(r));
		
		// verify failure
		checkForCorrectException(new whatToRun() { public @Override void run() {
			r.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"is closed");
		
	}
	
	/** Almost the same as testErlangRunner1 
	 * but arguments are mangled hence they fail to patternmatch in the server and 
	 * we receive a timeout.
	 */
	@Test
	public void testErlangResponseTimeout2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"timeout");
	}

	/** Almost the same as testErlangRunner1 
	 * but the server responds with {noreply, State}
	 */
	@Test
	public void testErlangResponseTimeout3()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(new OtpErlangObject[]{new OtpErlangAtom("noreply")},200);
		}},IllegalArgumentException.class,"timeout");
	}

	/** Checks for a timeout if Erlang process dies, by killing it.
	 * @throws InterruptedException 
	 */
	@Test
	public void testErlangResponseTimeout4() throws InterruptedException
	{
		checkEcho(runner);
		erlRuntime.erlangProcess.destroy();
		erlRuntime.erlangProcess.waitFor();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			makeCall(runner);
		}},IllegalArgumentException.class,"timeout");
		erlRuntime.killErlang();
	}
	
	/** Checks that we timeout after the first failure. Perhaps Erlang needs restarting in this case, not sure about this
	 * since such a failure should never occur in production use. */
	@Test
	public void testErlangResponseTimeout5()
	{
		checkEcho(runner);
		checkEcho(runner);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},200);
		}},IllegalArgumentException.class,"timeout");
		checkForCorrectException(new whatToRun() { public @Override void run() {
			makeCall(runner);
		}},IllegalArgumentException.class,"timeout");
		
	}
	
	/** Checks decoding of server responses by call() */
	@Test
	public void testCallResponseDecoding1()
	{
		
		OtpErlangTuple response = runner.call(
				new OtpErlangObject[]{new OtpErlangAtom("echo2Tuple"), new OtpErlangAtom("aaa")},
				"ErrMsg");
		Assert.assertEquals(new OtpErlangTuple(new OtpErlangObject[]{ErlangRunner.okAtom, new OtpErlangAtom("aaa"), new OtpErlangAtom("bbb")}),
				response);
	}

	/** Checks decoding of server responses by call() */
	@Test
	public void testCallResponseDecoding2()
	{
		
		OtpErlangTuple response = runner.call(
				new OtpErlangObject[]{new OtpErlangAtom("echo2Notuple_ok"), new OtpErlangAtom("aaa")},
				"ErrMsg");
		Assert.assertNull(response);
	}

	@Test
	public void testCallResponseDecodeFailure1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erlRuntime.createNewRunner().call(
					new OtpErlangObject[]{new OtpErlangAtom("echo2Error"), new OtpErlangAtom("aaa")},
					"ErrMsg");
		}},IllegalArgumentException.class,"ErrMsg : error errorProcessinG");
	}

	@Test
	public void testCallResponseDecodeFailure2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(
					new OtpErlangObject[]{new OtpErlangAtom("echo2ErrorMessage"), new OtpErlangAtom("aaa")},
					"ErrMsg");
		}},IllegalArgumentException.class,"veryLongErrorMessage");
	}
	
	@Test
	public void testCallResponseDecodeFailure3()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(
					new OtpErlangObject[]{new OtpErlangAtom("echo2List"), new OtpErlangAtom("aaa")},
					"ErrMsg");
		}},IllegalArgumentException.class,"unexpected response type");
	}
	
	@Test
	public void testCallResponseDecodeFailure4()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(
					new OtpErlangObject[]{new OtpErlangAtom("echo2WrongType"), new OtpErlangAtom("aaa")},
					"ErrMsg");
		}},IllegalArgumentException.class,"unexpected type in response tuple");
	}
	
	@Test
	public void testCallResponseDecodeFailure5()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.call(
					new OtpErlangObject[]{new OtpErlangAtom("echo2ShortTuple"), new OtpErlangAtom("aaa")},
					"ErrMsg");
		}},IllegalArgumentException.class,"unexpectedly short response");
	}
	
	@Test 
	public void testEvaluateTerm1()
	{
		Assert.assertEquals(new OtpErlangLong(25),runner.evaluateString("25"));
	}
	
	@Test 
	public void testEvaluateTerm2()
	{
		Assert.assertEquals("[{10,6},{7,3},{13,9}]",ErlangLabel.dumpErlangObject(runner.evaluateString("[{X+4,X} || X <- [6,3,9] ]")));
	}
	
	@Test 
	public void testEvaluateTermFailure1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			runner.evaluateString("aa/gg.");
		}},IllegalArgumentException.class,"syntax error before");
	}
	
}
