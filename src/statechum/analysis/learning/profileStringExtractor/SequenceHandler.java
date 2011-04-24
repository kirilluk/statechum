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

import java.util.*;

import javax.swing.tree.TreePath;

import org.xml.sax.Attributes;

import statechum.Configuration;

public class SequenceHandler extends AbstractHandler {
	
	protected Set<String> vocabulary; 
	
	public SequenceHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods, Configuration conf){
		super(functions, classMethods,conf);
		vocabulary = new HashSet<String>();
		computeVocab(functions.values());
	}
	
	private void computeVocab(Collection<List<TreePath>> functionDefinitions){
		for (List<TreePath> list : functionDefinitions) {
			List<String> stringPath = pathToStrings(list);
			vocabulary.addAll(stringPath);
		}
	}

	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) {
		if(qName.equals("methodEntry")){
			String methodIdRef = attributes.getValue("methodIdRef");
			String classIdRef = attributes.getValue("classIdRef");
			String ticketRef = attributes.getValue("ticket");
			if(!(methodIdRef == null) && !(classIdRef == null)&& !(ticketRef == null)){
				Integer methodId = Integer.valueOf(methodIdRef);
				Integer ticket = Integer.valueOf(ticketRef);
				String methodString = convertToString(methodId);
				methodSequence.add(ticket);
				ticketToString.put(ticket, convertToSignatureString(methodId));
				if(vocabulary.contains(methodString))
					checkSequenceForFunction(methodSequence);
			}
		}
		else if(qName.equals("methodExit")){
			String ticketRef = attributes.getValue("ticket");
			if(!(ticketRef == null)){
				Integer ticket = Integer.valueOf(ticketRef);
				methodSequence.add(ticket);
			}
		}
	}
	
	
	
	protected void checkSequenceForFunction(List<Integer> sequence){
		for(String key:functions.keySet()){
			List<TreePath> l = functions.get(key);
			if(containsString(sequence, pathToStrings(l))){
				functionString.add(key);
				methodSequence.clear();
			}
		}
	}

}
