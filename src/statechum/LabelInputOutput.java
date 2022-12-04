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
package statechum;

import java.util.Arrays;

/**
 * @author kirill
 *
 */
public class LabelInputOutput implements Label {

	public final String label;
	public final String input, output;
	public final boolean errorTransition;

	public LabelInputOutput(String l)
	{
		String[] input_output = l.split(" */ *");
		if (input_output.length != 2)
			throw new IllegalArgumentException("invalid format of label " + Arrays.toString(input_output));
		errorTransition =  input_output[1].equalsIgnoreCase("error");
		input = input_output[0];output = input_output[1];
		label = input+"/"+output;
	}

	public boolean isErrorTransition() {
		return errorTransition;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Label o) {
		return toErlangTerm().compareTo(o.toErlangTerm());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return input.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		return obj instanceof LabelInputOutput && input.equals(((LabelInputOutput)obj).input);
	}

	@Override
	public String toErlangTerm() {
		return "{"+input+","+output+"}";
	}

	@Override
	public String toString()
	{
		return label;
	}

	@Override
	public int toInt() {
		throw new UnsupportedOperationException("pairlabels do are not numbered by default, this label is "+label);
	}
}
