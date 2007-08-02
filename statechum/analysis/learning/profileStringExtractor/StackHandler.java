package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.Attributes;

import java.util.*;

import javax.swing.tree.TreePath;

public class StackHandler extends AbstractHandler {
	
	public StackHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods){
		super(functions, classMethods);
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
				Integer classId = Integer.valueOf(classIdRef);
				Integer ticket = Integer.valueOf(ticketRef);
				String methodString = convertToString(methodId);
				methodSequence.add(ticket);
				ticketToString.put(ticket, methodString);
				
			}
			
		}
		else if(qName.equals("methodExit")){
			Integer ticket = Integer.valueOf(attributes.getValue("ticket"));
			assert methodSequence.size() > 0 && ticket.equals(methodSequence.get(methodSequence.size()-1)): "inconsistent xml trace file";
			checkSequenceForFunction(methodSequence);
			methodSequence.remove(ticket);
			ticketToString.remove(ticket);
		}

	}
	
	protected void checkSequenceForFunction(List<Integer> sequence){
		for(String key:functions.keySet()){
			List<TreePath> l = functions.get(key);
			if(containsString(sequence, pathToStrings(l))){
				functionString.add(key);
			}
		}
	}

}
