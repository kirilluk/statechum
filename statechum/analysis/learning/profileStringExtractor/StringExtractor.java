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

package statechum.analysis.learning.profileStringExtractor;

import javax.swing.SwingUtilities;

public class StringExtractor {

	public static void main(String[] args) {
		final String[] commandlineArgs = args;
		final SplitFrame sFrame = new SplitFrame();

		if (args.length > 0)
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					sFrame.runAnExperiment(commandlineArgs);
				}
			});
	}

}
