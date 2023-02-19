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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

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
		throw new IllegalArgumentException(description+descr, e);
	}

	/** Loads the contents of a file into a string.
	 * @param file file to load
	 */
	public static String loadFile(File file) throws IOException
	{
		BufferedReader input = new BufferedReader(new FileReader(file));
		StringBuilder result = new StringBuilder();
		String line;
		while ((line = input.readLine()) != null) {
			result.append(line);result.append('\n');
		}
		input.close();return result.toString();
	}

}

