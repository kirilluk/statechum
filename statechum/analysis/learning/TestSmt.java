/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

import static statechum.Helper.checkForCorrectException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import statechum.Helper.whatToRun;

public class TestSmt {

	@BeforeClass
	public static void initSmtTest()
	{
		Smt.loadLibrary();
	}
	
	@Before
	public void before()
	{
		Smt.configureYices(-1, true);
	}
	
	/** Simple test, using multiple finalize. */
	@Test
	public void testYices1()
	{
		Smt smt = new Smt();
		
		smt.loadData("(define x::int)\n(assert (> x 1))");
		Assert.assertTrue(smt.check());
		smt.finalize();
		smt.finalize();
	}

	/** Push/pop. */
	@Test
	public void testYices2()
	{
		Smt smt = new Smt();
		
		smt.loadData("(define x::int)\n(assert (> x 1))");
		Assert.assertTrue(smt.check());
		smt.pushContext();
		smt.loadData("(assert (< x 0))");
		Assert.assertFalse(smt.check());
		smt.popContext();
		Assert.assertTrue(smt.check());
		smt.finalize();
	}
	
	/** Multiple contexts. */
	@Test
	public void testYices3()
	{
		Smt.closeStdOut();
		Smt smtA = new Smt();
		
		smtA.loadData("(define x::int)\n(assert (> x 1))");
		Assert.assertTrue(smtA.check());
		smtA.pushContext();
		smtA.loadData("(assert (< x 0))");
		
		Smt smtB = new Smt();
		smtB.loadData("(define x::int)\n(define y::int)\n(assert (< y 0))");
		Assert.assertTrue(smtB.check());
		Assert.assertFalse(smtA.check());
		Assert.assertTrue(smtB.check());
		smtA.popContext();
		smtB.loadData("(assert (> y 4))");
		Assert.assertTrue(smtA.check());
		Assert.assertFalse(smtB.check());
		smtA.finalize();
		Smt.reopenStdOut();
	}
	
	/** Cannot parse. */
	@Test
	public void testYices_error1()
	{
		
		checkForCorrectException(new whatToRun() { public void run() throws NumberFormatException {
			Smt smt = new Smt();smt.loadData("(define x::int)\n(assert (A> x 1))");
		}}, IllegalArgumentException.class,"Undefined name \"A>\"");
	}
	
	/** Checks that if type checking is not enabled, the error goes undetected. */
	@Test
	public void testYices_error2_not_reported()
	{
		Smt.configureYices(-1, false);
		Smt smt = new Smt();smt.loadData("(define x::bool)\n(assert (> x 1))");
	}
	
	/** Cannot parse - type error. */
	@Test
	public void testYices_error2()
	{
		checkForCorrectException(new whatToRun() { public void run() throws NumberFormatException {
			Smt smt = new Smt();smt.loadData("(define x::bool)\n(assert (> x 1))");
		}}, IllegalArgumentException.class,"argument is not a numeral");
	}
	
	/** Check if a field can be correctly extracted. */
	@Test(expected=IllegalArgumentException.class)
	public void testCorrectFieldExtraction0()
	{
		Smt.testGetField(null);		
	}
	
	/** Check if a field can be correctly extracted. */
	@Test(expected=NoSuchFieldError.class)
	public void testCorrectFieldExtraction1()
	{
		Smt.testGetField(new Object());		
	}
	
	/** Check if a field can be correctly extracted. */
	@Test(expected=NoSuchFieldError.class)
	public void testCorrectFieldExtraction2()
	{
		Smt.testGetField(new Object(){ @SuppressWarnings("unused") int context; });		
	}
	
	/** Check if a field can be correctly extracted. */
	@Test
	public void testCorrectFieldExtraction3()
	{
		Assert.assertTrue(Smt.testGetField(new Object(){ @SuppressWarnings("unused") long context; }));		
	}
}
