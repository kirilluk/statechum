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

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author kirill
 *
 */
public class LabelInputOutput implements Label {

	public final String label;
	public final String input, output;
	public final boolean errorTransition;
	public final boolean ioPair;

	int idx = -1;

	/** Creates an io label.
	 *
	 * @param l text to parse into a label
	 * @param allowPartialAutomata whether transitions with specific string can be seen as error-transitions, corresponding to input that do not exist.
	 * @param iopair whether the constructed label should have equality defined on both input and output (iopair being true) or on input only.
	 *               The former is good for learning transition systems where we would like transitions with the same
	 *               input and multiple different output to sink-states, reflecting what definitely cannot happen from some states.
	 *               This would be seen as very non-deterministic for input-equivalence only and would be significant
	 *               for the purpose of test generation.
	 */
	public LabelInputOutput(String l, boolean allowPartialAutomata, boolean iopair)
	{
		String[] input_output = l.split(" */ *");
		if (input_output.length != 2)
			throw new IllegalArgumentException("invalid format of label " + Arrays.toString(input_output));

		if (allowPartialAutomata)
			errorTransition =  input_output[1].equalsIgnoreCase(ERROR);
		else
			errorTransition = false;

		ioPair = iopair;

		input = input_output[0];
		if (errorTransition)
			output = ERROR;
		else if (input_output[1].equalsIgnoreCase(NO_OUTPUT))
			output = NO_OUTPUT;
		else
			output = input_output[1];

		label = input+"/"+output;
	}

	public static final String NO_OUTPUT="no_out";
	public static String ERROR="error";

	static void setErrorOutput(String errOutput) {
		ERROR = errOutput;
	}

	static String getErrorOutput() {
		return ERROR;
	}

	public LabelInputOutput(String i, String o, boolean allowPartialAutomata, boolean iopair) {
		input = i;output = o != null? o:NO_OUTPUT;

		if (allowPartialAutomata)
			errorTransition = Objects.equals(o, ERROR);
		else
			errorTransition = false;

		ioPair = iopair;
		label = input+"/"+output;
	}

	public boolean isErrorTransition() {
		return errorTransition;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Label other) {
		if (!(other instanceof LabelInputOutput))
			throw new IllegalArgumentException(
					"Comparing a LabelInputOutput to "+other.getClass()+" that not a LabelInputOutput");
		if (((LabelInputOutput)other).ioPair != ioPair)
			throw new IllegalArgumentException(
					"Comparing a LabelInputOutput (ioPair="+ioPair+") to LabelInputOutput (ioPair="+((LabelInputOutput)other).ioPair+")");
		if (ioPair)
		  return label.compareTo( ((LabelInputOutput)other).label);
		else
		  return input.compareTo( ((LabelInputOutput)other).input );
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		if (ioPair)
			return label.hashCode();
		return input.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (!(obj instanceof LabelInputOutput))
			throw new IllegalArgumentException("Cannot compare LabelInputOutput to "+obj.getClass().getName());
		if (!input.equals(((LabelInputOutput)obj).input))
			return false;

		return !ioPair || output.equals(((LabelInputOutput)obj).output);
	}

	@SuppressWarnings("BooleanMethodIsAlwaysInverted")
	public boolean deepEquals(Object obj) {
		if (!(equals(obj)))
			return false;

		LabelInputOutput other = (LabelInputOutput) obj;
		return Objects.equals(output, other.output);
	}

	public static boolean deepEqualsCollection(Collection<LabelInputOutput> a,Collection<LabelInputOutput> b) {
		if (a.size() != b.size())
			return false;

		Iterator<LabelInputOutput> a_iter = a.iterator(),b_iter = b.iterator();
		while (a_iter.hasNext()) {
			if (!a_iter.next().deepEquals(b_iter.next()))
				return false;
		}
		return true;
	}

	public static String deepDiffCollection(Collection<LabelInputOutput> a,Collection<LabelInputOutput> b) {
		if (a.size() != b.size())
			return "Sequence are of different length";

		Iterator<LabelInputOutput> a_iter = a.iterator(),b_iter = b.iterator();
		int position = 0;
		while (a_iter.hasNext() && b_iter.hasNext()) {
			LabelInputOutput a_elem = a_iter.next(), b_elem = b_iter.next();
			if (!a_elem.deepEquals(b_elem))
				return "Elements in position "+position+" (starting from 0), "+a_elem+" v.s. "+b_elem;
			++position;
		}
		return null;
	}

	public static List<String> toInput(Collection<Label> seq) {
		return seq.stream().map(k -> ((LabelInputOutput)k).input).collect(Collectors.toList());
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

	public void setIdx(int value) {
		idx = value;
	}

	@Override
	public int toInt() {
		if (idx < 0)
			throw new UnsupportedOperationException("pairlabels do are not numbered by default, this label is "+label);
		return idx;
	}
}
