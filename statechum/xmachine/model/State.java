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

public class State {

	private String label;

	private MemState memory;

	boolean start, term;

	public State(String label) {
		this.label = label;
	}

	public MemState getMemory() {
		return memory;
	}

	public void setMemory(MemState memory) {
		this.memory = memory;
	}

	public String getLabel() {
		return label;
	}

	public boolean equals(State s) {
		if (s.getLabel().equals(this.getLabel())) {
			return true;
		} else
			return false;
	}

	public boolean isStart() {
		return start;
	}

	public void setStart(boolean start) {
		this.start = start;
	}

	public boolean isTerm() {
		return term;
	}

	public void setTerm(boolean term) {
		this.term = term;
	}

}
