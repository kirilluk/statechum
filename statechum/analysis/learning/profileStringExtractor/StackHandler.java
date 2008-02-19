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

package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.Attributes;

import java.util.*;

import javax.swing.tree.TreePath;

public class StackHandler extends AbstractHandler {

	public StackHandler(Map<String, List<TreePath>> functions,
			ClassMethodDefsHandler classMethods) {
		super(functions, classMethods);
	}

	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) {
		if (qName.equals("methodEntry")) {
			String methodIdRef = attributes.getValue("methodIdRef");
			String classIdRef = attributes.getValue("classIdRef");
			String ticketRef = attributes.getValue("ticket");
			if (!(methodIdRef == null) && !(classIdRef == null)
					&& !(ticketRef == null)) {
				Integer methodId = Integer.valueOf(methodIdRef);
				Integer classId = Integer.valueOf(classIdRef);
				Integer ticket = Integer.valueOf(ticketRef);
				String methodString = convertToString(methodId);
				methodSequence.add(ticket);
				ticketToString.put(ticket, methodString);

			}

		} else if (qName.equals("methodExit")) {
			Integer ticket = Integer.valueOf(attributes.getValue("ticket"));
			assert methodSequence.size() > 0
					&& ticket.equals(methodSequence
							.get(methodSequence.size() - 1)) : "inconsistent xml trace file";
			checkSequenceForFunction(methodSequence);
			methodSequence.remove(ticket);
			ticketToString.remove(ticket);
		}

	}

	protected void checkSequenceForFunction(List<Integer> sequence) {
		for (String key : functions.keySet()) {
			List<TreePath> l = functions.get(key);
			if (containsString(sequence, pathToStrings(l))) {
				functionString.add(key);
			}
		}
	}

}
