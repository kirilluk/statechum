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

package statechum.analysis.learning.findloops;

import statechum.analysis.learning.profileStringExtractor.*;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import java.util.*;
import java.io.*;

public class TranslatorParser extends DefaultHandler {

	private Map<Integer, Integer> ticketToClassID, classIDtoProcess;

	private Integer highestClassID, process;

	private ClassMethodDefsHandler classMethods;

	private List<Integer> methodStack;

	private List<String> stringList;

	private FileOutputStream fos;

	private OutputStreamWriter out;

	public TranslatorParser(String outputName,
			ClassMethodDefsHandler classMethods) {
		this.stringList = new ArrayList<String>();
		this.classMethods = classMethods;
		this.ticketToClassID = new HashMap<Integer, Integer>();
		this.methodStack = new ArrayList<Integer>();
		this.highestClassID = new Integer(0);
		this.classIDtoProcess = new HashMap<Integer, Integer>();
		this.process = 1;
		try {
			this.fos = new FileOutputStream(outputName);
			this.out = new OutputStreamWriter(fos, "UTF-8");
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(0);
		}
	}

	/**
	 * Findloops seems to need process IDs to start at 1, and iterate from
	 * there. For this reason we need to establish a mapping from the arbitrary
	 * TPTP class IDs to iterative consecutive process IDs.
	 * 
	 * @param classId
	 * @return
	 */
	private Integer getMappingInteger(Integer classId) {
		if (this.classIDtoProcess.containsKey(classId))
			return this.classIDtoProcess.get(classId);
		else {
			this.classIDtoProcess.put(classId, this.process);
			process++;
			return process - 1;
		}
	}

	@Override
	public void endDocument() throws SAXException {
		super.endDocument();
		try {
			out.write(this.highestClassID.toString() + "\n");
			Iterator<String> stringArrayIt = stringList.iterator();
			while (stringArrayIt.hasNext()) {
				out.write(stringArrayIt.next());
			}
			out.close();
			fos.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void startElement(String uri, String localName, String qName,
			Attributes attributes) {
		// KIRR: why are we checking at method exit rather than entry?
		// Are we not getting the sequence of calls in reverse if calls are
		// nested?
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
				if (methodStack.size() > 0)
					addToFindloopsObservation(classId, methodId, methodString);
				methodStack.add(ticket);
				ticketToClassID.put(ticket, classId);

			}

		} else if (qName.equals("methodExit")) {
			Integer ticket = Integer.valueOf(attributes.getValue("ticket"));
			assert methodStack.size() > 0
					&& ticket.equals(methodStack.get(methodStack.size() - 1)) : "inconsistent xml trace file";
			methodStack.remove(ticket);
			ticketToClassID.remove(ticket);
		}
	}

	private void addToFindloopsObservation(Integer classId, Integer methodId,
			String methodString) {
		Integer stackTicket = methodStack.get(methodStack.size() - 1);
		Integer senderClassID = ticketToClassID.get(stackTicket);
		Integer processForSenderClass = getMappingInteger(senderClassID);
		Integer processForCurrentClass = getMappingInteger(classId);
		stringList.add("s" + "\t" + processForSenderClass + "\t"
				+ processForCurrentClass + "\t" + methodString + "\n");
		stringList.add("r" + "\t" + processForSenderClass + "\t"
				+ processForCurrentClass + "\t" + methodString + "\n");
		if (processForSenderClass > this.highestClassID)
			highestClassID = processForSenderClass;
		if (processForCurrentClass > highestClassID)
			highestClassID = processForCurrentClass;
	}

	/**
	 * Every method has a unique id, this function finds a class corresponding
	 * to this method and returns a full name of this method.
	 */
	private String convertToString(Integer methodId) {
		Map<Integer, String> classDefs = classMethods.getClassDefs();
		Map<Integer, String> methodDefs = classMethods.getMethodDefs();
		Map<Integer, Set<Integer>> classesToMethods = classMethods
				.getClassesToMethods();
		Integer classId = findKeyFor(classesToMethods, methodId);
		String className = (String) classDefs.get(classId);
		String methodName = (String) methodDefs.get(methodId);
		return className + "." + methodName;
	}

	private Integer findKeyFor(Map<Integer, Set<Integer>> classesToMethods,
			Integer method) {
		Iterator keyIt = classesToMethods.keySet().iterator();
		while (keyIt.hasNext()) {
			Integer nextKey = (Integer) keyIt.next();
			Collection methods = classesToMethods.get(nextKey);
			if (methods.contains(method))
				return nextKey;
		}
		return null;
	}
}
