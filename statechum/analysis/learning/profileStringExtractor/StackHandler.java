package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import java.util.*;

import javax.swing.tree.TreePath;

public class StackHandler extends DefaultHandler {
	
	private Map<Integer,String> ticketToString;
	private Map<String,List<TreePath>> functions;
	private Set<Integer> doneTickets;
	private ClassMethodDefsHandler classMethods;
	private List<Integer> methodStack;
	private List<String> functionString;
	
	public StackHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods){
		this.functionString = new ArrayList<String>();
		this.classMethods = classMethods;
		this.functions = functions;
		this.ticketToString = new HashMap<Integer,String>();
		this.methodStack = new ArrayList<Integer>();
		this.doneTickets = new HashSet<Integer>();
	}
	
	public String getFunctionString(int maxLoopSize){
		String returnString = "{";
		int counter=0;
		for(int i=0;i<functionString.size();i++){
			if(i==functionString.size()-1)
				returnString = returnString.concat("\""+functionString.get(i)+"\"}");
			else if(i>0){
				String s = functionString.get(i);
				if(s.equals(functionString.get(i-1)))
					counter++;
				else
					counter = 0;
				if(counter<maxLoopSize)
					returnString = returnString.concat("\""+s+"\",");
			}
			else returnString = returnString.concat("\""+functionString.get(i)+"\",");
				
		}
		return returnString;
	}
	
	public ArrayList<String> getArrayListFunctionString(int maxLoopSize){
		ArrayList<String> string = new ArrayList<String>();
		int counter=0;
		for(int i=0;i<functionString.size();i++){
			String s = functionString.get(i);
			if(i>0){
				if(s.equals(functionString.get(i-1)))
					counter++;
				else
					counter = 0;
				}
			if(counter<maxLoopSize)
				string.add(s);
		}
	return string;
	}
	
	public void startElement(String uri, String localName, String qName, Attributes attributes){
		// KIRR: why are we checking at method exit rather than entry? 
		// Are we not getting the sequence of calls in reverse if calls are nested?
		if(qName.equals("methodEntry")){
			String methodIdRef = attributes.getValue("methodIdRef");
			String classIdRef = attributes.getValue("classIdRef");
			String ticketRef = attributes.getValue("ticket");
			if(!(methodIdRef == null) && !(classIdRef == null)&& !(ticketRef == null)){
				Integer methodId = Integer.valueOf(methodIdRef);
				Integer classId = Integer.valueOf(classIdRef);
				Integer ticket = Integer.valueOf(ticketRef);
				String methodString = convertToString(methodId);
				methodStack.add(ticket);
				ticketToString.put(ticket, methodString);
				
			}
			
		}
		else if(qName.equals("methodExit")){
			Integer ticket = Integer.valueOf(attributes.getValue("ticket"));
			assert methodStack.size() > 0 && ticket.equals(methodStack.get(methodStack.size()-1)): "inconsistent xml trace file";
			checkStackForFunction(methodStack);
			methodStack.remove(ticket);
			ticketToString.remove(ticket);
		}
	}
	
	private void checkStackForFunction(List<Integer> methodStack){
		Iterator<String> functionIt =  functions.keySet().iterator();
		while(functionIt.hasNext()){
			String key = functionIt.next();
			List<TreePath> l = functions.get(key);
			if(containsString(methodStack, pathToStrings(l)))
				functionString.add(key);
		}
	}
	
	private boolean containsString(List<Integer> stack, List<String> list){
		for(int index=0;index<stack.size();index++){
			Integer ticket = stack.get(index);
			if(doneTickets.contains(ticket))
				continue;
			String methodString = ticketToString.get(ticket);
			if(methodString.equals(list.get(0).trim())){
				if(list.size() == 1){
					doneTickets.addAll(stack);
					return true;
				}
				else
					return containsString(stack.subList(index+1, stack.size()), list.subList(1, list.size()));
			}
			
		}
		return false;
	}
	
	/** Given a list of paths, this function returns a list of textual representations of those paths. */
	private static List<String> pathToStrings(List<TreePath> list){
		Iterator<TreePath> listIt = list.iterator();
		List<String> returnList = new ArrayList<String>();
		for(int i=0;i<list.size();i++){
			TreePath current = listIt.next();
			String pathString = new String();
			for(int j=1;j<current.getPathCount();j++){
				if(j<current.getPathCount()-1)
					pathString = pathString.concat(current.getPathComponent(j).toString()+".");
				else
					pathString = pathString.concat(current.getPathComponent(j).toString());
			}
			
			returnList.add(pathString);
		}
		return returnList;
	}

	/** Every method has a unique id, this function finds a class corresponding 
	 * to this method and returns a full name of this method. 
	 */
	private String convertToString(Integer methodId){
		Map<Integer, String> classDefs = classMethods.getClassDefs();
		Map<Integer, String> methodDefs = classMethods.getMethodDefs();
		Map<Integer, Set<Integer>> classesToMethods = classMethods.getClassesToMethods();
		Integer classId = findKeyFor(classesToMethods,methodId);
		String className = (String)classDefs.get(classId);
		String methodName = (String)methodDefs.get(methodId);
		return className+"."+methodName;
	}
	
	private Integer findKeyFor(Map<Integer, Set<Integer>> classesToMethods, Integer method){
		Iterator keyIt = classesToMethods.keySet().iterator();
		while(keyIt.hasNext()){
			Integer nextKey = (Integer)keyIt.next();
			Collection methods = classesToMethods.get(nextKey);
			if(methods.contains(method))
				return nextKey;
		}
		return null;
	}
}
