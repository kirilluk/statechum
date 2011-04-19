/* Copyright (c) 2011 Ramsay Taylor and Kirill Bogdanov
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

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;

public class TestErlangModule {

	@Test
	public void testGetErlName1()
	{
		Assert.assertEquals("aa",ErlangRunner.getErlName("aa.erl"));
		Assert.assertEquals("aa",ErlangRunner.getErlName("  aa.erl   "));
	}

	@Test
	public void testGetErlName2()
	{
		Assert.assertEquals("aa/bb",ErlangRunner.getErlName("  aa/bb.erl   "));
		Assert.assertEquals("aa/bb...",ErlangRunner.getErlName("  aa/bb....erl   "));
		Assert.assertEquals("aa/bb.c.d",ErlangRunner.getErlName("  aa/bb.c.d.erl   "));
		Assert.assertEquals("aa/bb.c.d.erl",ErlangRunner.getErlName("  aa/bb.c.d.erl.erl   "));
	}
	
	@Test
	public void testGetErlNameFail()
	{
		Assert.assertNull(ErlangRunner.getErlName(".erl"));
		Assert.assertNull(ErlangRunner.getErlName(null));
		Assert.assertNull(ErlangRunner.getErlName("aa.berl"));
		Assert.assertNull(ErlangRunner.getErlName("aa.berl"));
		Assert.assertNull(ErlangRunner.getErlName("aa.erl.tt"));
		Assert.assertNull(ErlangRunner.getErlName("aa"));
	}
	
	@Test
	public void testRunParserFailure1()
	{
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public @Override void run() throws IOException {
			new ErlangModule(new java.io.File("ErlangExamples/WibbleMonster/wibble.erla"));
		}},IllegalArgumentException.class,"invalid Erlang file name");
	}

	@Test
	public void testRunParserFailure2()
	{
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { public @Override void run() throws IOException {
			new ErlangModule(new java.io.File("ErlangExamples/WibbleMonster/Wibble.erl"));
		}},IllegalArgumentException.class,"does not match file name");
	}

	@Test
	public void testRunParser1() throws IOException
	{
		ErlangModule mod = new ErlangModule(new java.io.File("ErlangExamples/WibbleMonster/wibble.erl"));
		Assert.assertEquals("[{call, xyz, '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {call, [wibble], '*'}, {call, [wibble], '*'}, {call, [wibble], '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {cast, wibble}, {cast, stop}, {cast, xyz}, {cast, xyz3}, {info, wibble}, {info, xyz}]",
mod.behaviour.alphabet.toString());
	}
}
