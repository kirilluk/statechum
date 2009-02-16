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

import java.util.List;

import statechum.Pair;

public interface AbstractOracle {
	/** Used in an interface to indicate that a user wishes to abort the leaning process. */
	public static final int USER_CANCELLED = -2;
	
	/** Means that the path is a possible one - invalid paths are indicated with
	 *  non-negative integers reflecting the position in the question of the first 
	 *  input which is not possible. This way, a response of zero means that the 
	 *  first input is not possible from the initial state of the model being learnt. 
	 */
	public static final int USER_ACCEPTED = -3;
	
	/** Means that a user wishes to provide an LTL expression to answer this and more questions. 
	 *TODO: change the type to an Object so that one could offer LearnerGraphs to be merged into the model being learnt (reflecting the paper on a "more expressive teacher in L*")
	 */
	public static final int USER_LTL = -4;
	
	/** Means that a user offered an if-then automaton. */
	public static final int USER_IFTHEN = -5;
	
	/** Means that a user chose to ignore this question, hence we assume that the answer was
	 * compatible with the chosen state pair and not to add the supplied path to a PTA.
	 */
	public static final int USER_IGNORED = -6;
	
	/** Default value for a dialog box. */
	public static final int USER_WAITINGFORSELECTION = -1;

	/** Retrieves a stored answer. 
	 * This can either be a number corresponding to accept/reject (in which case the string part should be null) or 
	 * USER_LTL in the integer part and a string representation of an LTL formula.
	 * <p>
	 * Returns null if an oracle does not know an answer - useful for nesting multiple oracles.
	 */
	public Pair<Integer,String> getAnswer(List<String> question);
}
