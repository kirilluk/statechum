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

import java.io.*;

import org.junit.Assert;

import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;

/** Contains universally-useful methods, all of them static.
 * 
 * @author kirill
 *
 */
public class Helper {

	/** Used extensively to convert checked exceptions to IllegalArgumentException
	 * (obviously in places where no exceptions should occur hence no need to use
	 * checked exceptions).
	 * 
	 * @param description description of why the exception is to be thrown
	 * @param e exception to convert
	 */
	public static void throwUnchecked(String description, Throwable e)
	{
		String descr = e.getMessage() == null? "":(": "+e.getMessage());
		IllegalArgumentException ex = new IllegalArgumentException(description+descr);ex.initCause(e);
		throw ex;
	}

	public static final void checkForCorrectException(whatToRun what, Class<? extends Exception> exceptionClass, String exceptionString)
	{
		try
		{
			what.run();
			Assert.fail("Exception not thrown");
		}
		catch(Exception ex)
		{
			StringWriter str = new StringWriter();ex.printStackTrace(new PrintWriter(str));
			Assert.assertEquals("wrong type of exception received "+str.toString()+" instead of "+exceptionClass,exceptionClass,ex.getClass());
			if (ex.getMessage() == null)
				Assert.assertNull("got null instead of \""+exceptionString+"\"",exceptionString);
			else
			{
				if (exceptionString != null) 
					Assert.assertTrue("expected exception containing \""+exceptionString+"\" but got \""+ex.getMessage()+"\"",ex.getMessage().contains(exceptionString));
			}
		}
	}

	/** Loads the contents of a file into a string.
	 * @param file file to load
	 *
	 * @throws IOException
	 */
	public static String loadFile(File file) throws IOException
	{
		BufferedReader input = new BufferedReader(new FileReader(file));
		StringBuffer result = new StringBuffer();
		String line;
		while ((line = input.readLine()) != null) {
			result.append(line);result.append('\n');
		}
		input.close();return result.toString();
	}

	public interface whatToRun
	{
		public void run() throws NumberFormatException, java.io.IOException, IncompatibleStatesException;
	}
}

