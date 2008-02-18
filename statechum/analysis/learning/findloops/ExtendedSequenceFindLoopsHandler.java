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

import java.io.*;
import java.util.*;

import javax.swing.tree.TreePath;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import statechum.analysis.learning.profileStringExtractor.*;

public class ExtendedSequenceFindLoopsHandler extends SequenceHandler {

	private Map<Integer, Integer> ticketToClassID;

	private Map<List<Integer>, List<Integer>> observationsToProcesses;

	private Map<String, List<List<String>>> abstractFunctionsToObservations;

	private Map<List<String>, List<Integer>> stringListToIntegerList;

	private String triggerKey = null;

	private List<Integer> sequenceClone = new ArrayList<Integer>();

	public ExtendedSequenceFindLoopsHandler(
			Map<String, List<TreePath>> functions,
			ClassMethodDefsHandler classMethods) {
		super(functions, classMethods);
		this.ticketToClassID = new HashMap<Integer, Integer>();
		this.abstractFunctionsToObservations = new HashMap<String, List<List<String>>>();
		this.observationsToProcesses = new HashMap<List<Integer>, List<Integer>>();
		this.stringListToIntegerList = new HashMap<List<String>, List<Integer>>();
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
				Integer ticket = Integer.valueOf(ticketRef);
				Integer classId = Integer.valueOf(classIdRef);
				String methodString = convertToString(methodId);
				methodSequence.add(ticket);
				ticketToString.put(ticket, methodString);
				ticketToClassID.put(ticket, classId);
				if (vocabulary.contains(methodString)) {
					checkSequenceForFunction(methodSequence);
					if (triggerKey != null)
						addToFindloopsObservation();
				}
			}
		}
	}

	@Override
	protected void checkSequenceForFunction(List<Integer> sequence) {
		for (String key : functions.keySet()) {
			List<TreePath> l = functions.get(key);
			List<String> stringL = pathToStrings(l);
			if (containsString(sequence, stringL)) {
				functionString.add(key);
				this.triggerKey = key;
				this.sequenceClone.addAll(methodSequence);
				methodSequence.clear();
			}
		}
	}

	/**
	 * something wrong here.
	 * 
	 * @param classId
	 * @return
	 */
	private Integer getMappingInteger(Integer classId) {
		int index = 0;
		List<Integer> processesForFunction = observationsToProcesses
				.get(sequenceClone);
		if (processesForFunction == null) {
			processesForFunction = new ArrayList<Integer>();
			processesForFunction.add(classId);
			observationsToProcesses.put(sequenceClone, processesForFunction);
			return 1;
		} else
			index = processesForFunction.indexOf(classId);
		if (index < 0) {
			processesForFunction.add(classId);
			observationsToProcesses.put(sequenceClone, processesForFunction);
			return processesForFunction.indexOf(classId) + 1;
		}
		return index + 1;
	}

	private void addToFindloopsObservation() {
		List<List<String>> stringList = abstractFunctionsToObservations
				.get(triggerKey);
		if (stringList == null)
			stringList = new ArrayList<List<String>>();
		List<String> observation = new ArrayList<String>();
		for (int i = 1; i < sequenceClone.size(); i++) {
			Integer currentTicket = sequenceClone.get(i);
			Integer senderTicket = sequenceClone.get(i - 1);
			Integer senderClassId = ticketToClassID.get(senderTicket);
			Integer currentClassId = ticketToClassID.get(currentTicket);
			Integer processForSenderClass = getMappingInteger(senderClassId);
			Integer processForCurrentClass = getMappingInteger(currentClassId);
			observation.add("s" + "\t" + processForSenderClass + "\t"
					+ processForCurrentClass + "\t"
					+ ticketToString.get(currentTicket) + "\n");
			observation.add("r" + "\t" + processForSenderClass + "\t"
					+ processForCurrentClass + "\t"
					+ ticketToString.get(currentTicket) + "\n");
		}
		stringList.add(observation);
		this.stringListToIntegerList.put(observation, observationsToProcesses
				.get(sequenceClone));
		abstractFunctionsToObservations.put(triggerKey, stringList);
		triggerKey = null;
		this.sequenceClone.clear();
	}

	@Override
	public void endDocument() throws SAXException {
		super.endDocument();
		Iterator<String> keyIt = this.abstractFunctionsToObservations.keySet()
				.iterator();
		while (keyIt.hasNext()) {
			String key = keyIt.next();
			List<List<String>> observations = abstractFunctionsToObservations
					.get(key);
			int counter = 0;
			for (List<String> list : observations) {
				counter++;
				try {
					FileOutputStream fos = new FileOutputStream(
							"findloopsInput"
									+ System.getProperty("path.separator")
									+ key + counter);
					OutputStreamWriter out = new OutputStreamWriter(fos,
							"UTF-8");
					out.write(stringListToIntegerList.get(list).size() + "\n");
					for (String string : list) {
						out.write(string);
					}
					out.close();
					fos.close();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

}
