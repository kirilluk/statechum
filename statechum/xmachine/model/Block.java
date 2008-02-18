/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of statechum.

statechum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.xmachine.model;

/**
 * This is intended to represent a basic block of source code. As a result of
 * reverse engineering state machines from source code, we aim to be able to
 * attach basic blocks of source code to the transitions that govern their
 * execution (See TAIC PART paper). This class lets you specify the source code
 * that corresponds to a basic block in terms of the method signature and the
 * range of line numbers that belong to it.
 * 
 * @author Neil Walkinshaw
 * 
 */

public class Block {

	private String methodSignature;

	private int fromLine, toLine;

	public Block(String methodSignature) {
		this.methodSignature = methodSignature;
	}

	public String getMethodSignature() {
		return methodSignature;
	}

	public int getFromLine() {
		return fromLine;
	}

	public void setFromLine(int fromLine) {
		this.fromLine = fromLine;
	}

	public int getToLine() {
		return toLine;
	}

	public void setToLine(int toLine) {
		this.toLine = toLine;
	}

	public String toString() {
		String returnString = methodSignature + ": " + fromLine + " - "
				+ toLine;
		return returnString;
	}

	public boolean equals(Object anObject) {
		if (anObject instanceof Block) {
			Block blockObject = (Block) anObject;
			if (this.fromLine == blockObject.getFromLine()
					&& (this.toLine == blockObject.getToLine())
					&& this.methodSignature.equals(blockObject
							.getMethodSignature()))
				return true;
		}
		return false;
	}

}
