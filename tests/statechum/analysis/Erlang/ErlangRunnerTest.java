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
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

import statechum.Helper.whatToRun;
import statechum.apps.ErlangQSMOracle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangRunnerTest {
	
	/** Tests the echo method of the server, i.e.
	 * { dataHead, ['Data2',data3 ] } = gen_server:call(echo,[ dataHead, 'Data2', data3 ]) 
	 */
	@Test
	public void testErlangRunner1()
	{
		ErlangRunner erl = new ErlangRunner();
		String dataHead = "dataHead", dataB = "Data2", dataC = "data3";
		OtpErlangTuple tuple = (OtpErlangTuple)erl.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
				new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead), new OtpErlangAtom(dataB), new OtpErlangAtom(dataC)})});
		Assert.assertEquals(dataHead,((OtpErlangAtom)tuple.elementAt(0)).atomValue());
		OtpErlangObject [] list = ((OtpErlangList)tuple.elementAt(1)).elements();
		Assert.assertEquals(2, list.length);
		Assert.assertEquals(dataB,((OtpErlangAtom)list[0]).atomValue());
		Assert.assertEquals(dataC,((OtpErlangAtom)list[1]).atomValue());
	}
	
	@Test
	public void testCompileFailure1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(new File("junk"));
		}},IllegalArgumentException.class,"Failure running erlc");
	}
	
	@Test
	public void testCompileFailure2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangRunner.compileErl(new File("junk.erl"));
		}},IllegalArgumentException.class,"Failure running erlc");
	}
	
	@Test
	public void testCompile() throws IOException
	{
		ErlangRunner.compileErl(new File(ErlangQSMOracle.ErlangFolder,"tracerunner.erl"));
	}
	
	@Test
	public void testStartErlangFailure1a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("halt", 0);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure1b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("halt", 1000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure2a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("error", 0);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure2b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("error", 1000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure3a()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("nomatch", 0);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailure3b()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new ErlangRunner().startErlang("nomatch", 1000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	@Test
	public void testStartErlangFailureResponseTimeout1()
	{
		final ErlangRunner erl = new ErlangRunner();
		erl.startErlang("noserver", 0);
		final String dataHead = "dataHead", dataB = "Data2", dataC = "data3";
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erl.call(new OtpErlangObject[]{new OtpErlangAtom("echo"),
					new OtpErlangList(new OtpErlangObject[]{ new OtpErlangAtom(dataHead), new OtpErlangAtom(dataB), new OtpErlangAtom(dataC)})}
					,2000);
		}},IllegalArgumentException.class,"timeout");
	}
	
	/** Almost the same as testErlangRunner1 
	 * but arguments are mangled hence they fail to patternmatch in the server and 
	 * we receive a timeout.
	 */
	@Test
	public void testErtestStartErlangFailureResponseTimeout1langRunner1()
	{
		final ErlangRunner erl = new ErlangRunner();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			erl.call(new OtpErlangObject[]{new OtpErlangAtom("echoA")},2000);
		}},IllegalArgumentException.class,"timeout");
	}
}
