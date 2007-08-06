package statechum.analysis.learning.profileStringExtractor;

import java.util.*;

import javax.swing.tree.TreePath;

import org.xml.sax.Attributes;

public class SequenceHandler extends AbstractHandler {
	
	protected Set<String> vocabulary; 
	
	public SequenceHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods){
		super(functions, classMethods);
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
