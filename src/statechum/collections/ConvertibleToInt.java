/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 
package statechum.collections;

/** This interface is artificially different to {@link VertexConvertibleToInt} in order to make sure
 * I do not pass a vertex as a label.
 */
public interface ConvertibleToInt 
{
	/** Java collections consume much more memory than needed (through the use of Entry objects) 
	 * because each object holds 16 bytes more data than the fields it contains. 
	 * One way around this problem is to reduce the number of objects
	 * by using arrays, indexed with integers.
	 *   
	 * @return an integer representation of this object.
	 */
	public int toInt();
}
