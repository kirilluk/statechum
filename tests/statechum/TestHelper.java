/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum;

import org.junit.Assert;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

import java.io.*;
import java.util.List;

/** Contains universally-useful methods, all of them static.
 * 
 * @author kirill
 *
 */
public class TestHelper {

	public static void checkForCorrectException(whatToRun what, Class<? extends Exception> exceptionClass, String exceptionString)
	{
		try
		{
			what.run();
			Assert.fail("Exception not thrown");
		}
		catch(Exception ex)
		{
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong type of exception received "+ str +" instead of "+exceptionClass,exceptionClass,ex.getClass());
			if (ex.getMessage() == null)
				Assert.assertNull("got null instead of \""+exceptionString+"\"",exceptionString);
			else
				if (exceptionString != null)
					Assert.assertTrue("expected exception containing \""+exceptionString+"\" but got \""+ex.getMessage()+"\"",ex.getMessage().contains(exceptionString));
		}
	}
	public static void checkForCorrectExceptionAnyOf(whatToRun what, Class<? extends Exception> exceptionClass, List<String> exceptionString)
	{
		try
		{
			what.run();
			Assert.fail("Exception not thrown");
		}
		catch(Exception ex)
		{
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong type of exception received "+ str +" instead of "+exceptionClass,exceptionClass,ex.getClass());
			if (ex.getMessage() == null)
				Assert.fail("got null instead of the provided strings "+exceptionString);
			else
			if (exceptionString != null) {
				for(String s:exceptionString)
					if (ex.getMessage().contains(s))
						return;
				Assert.fail("expected exception containing any of \"" + exceptionString + "\" but got \"" + ex.getMessage() + "\"");
			}
		}
	}

	public static class ExceptionAndStrings {
		Class<? extends Exception> expectedException;
		List<String> expectedText;

		public ExceptionAndStrings(Class<? extends Exception> expected, List<String> text) {
			expectedException = expected;expectedText = text;
		}
	}

	public static void checkForCorrectExceptionAnyOf(whatToRun what, List<ExceptionAndStrings> expected)
	{
		try
		{
			what.run();
			Assert.fail("Exception not thrown");
		}
		catch(Exception ex)
		{
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			boolean textFound = false;
			for(ExceptionAndStrings e:expected) {
				if (ex.getClass() == e.expectedException) {
					if (e.expectedText == null && ex.getMessage() == null) {
						textFound = true;
						break;
					}

					if (e.expectedText != null && ex.getMessage() != null)
						for(String possibleText:e.expectedText)
							if (ex.getMessage().contains(possibleText)) {
								textFound = true;break;
							}

					if (textFound)
						break;
				}
			}

			Assert.assertTrue("wrong type of exception received or exception did not contained the expected text, got: "+
					ex.getClass()+" with text "+ (ex.getMessage() == null?"null":"\""+ex.getMessage()+"\""), textFound);
		}
	}
	public interface whatToRun
	{
		void run() throws NumberFormatException, java.io.IOException, IncompatibleStatesException;
	}
}

