package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import java.util.*;

import javax.swing.tree.TreePath;

public class StackHandler extends DefaultHandler {
	
	private HashMap functions, ticketToString;
	private HashSet doneTickets;
	private ClassMethodDefsHandler classMethods;
	private List functionString, methodStack;
	
	public StackHandler(HashMap functions, ClassMethodDefsHandler classMethods){
		this.functionString = new ArrayList();
		this.classMethods = classMethods;
		this.functions = functions;
		this.ticketToString = new HashMap();
		this.methodStack = new ArrayList<String>();
		this.doneTickets = new HashSet();
	}
	
	public String getFunctionString(int maxLoopSize){
		String returnString = "{";
		int counter=0;
		for(int i=0;i<functionString.size();i++){
			if(i==functionString.size()-1)
				returnString = returnString.concat("\""+functionString.get(i)+"\"}");
			else if(i>0){
				String s = (String)functionString.get(i);
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
	
	public ArrayList getArrayListFunctionString(int maxLoopSize){
		ArrayList string = new ArrayList();
		int counter=0;
		for(int i=0;i<functionString.size();i++){
			String s = (String)functionString.get(i);
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
			if(methodStack.size()>0){
				int index = methodStack.indexOf(ticket);
				if(index == methodStack.size()-1){
					if(ticket.equals(methodStack.get(methodStack.size()-1)))
						checkStackForFunction(methodStack);
					methodStack.remove(ticket);
					ticketToString.remove(ticket);
				}
				else if(index<0)
					return;
				else{
					System.out.println("inconsistent");
				}
				
			}
		}
	}
	
	private void checkStackForFunction(List methodStack){
		Iterator functionIt =  functions.keySet().iterator();
		while(functionIt.hasNext()){
			String key = (String)functionIt.next();
			List l = (List)functions.get(key);
			if(containsString(methodStack, pathToStrings(l)))
				functionString.add(key);
		}
	}
	
	private boolean containsString(List<Integer> stack, List<String> list){
		for(int index=0;index<stack.size();index++){
			Integer ticket = stack.get(index);
			if(doneTickets.contains(ticket))
				continue;
			String methodString = (String)ticketToString.get(ticket);
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

	
	private List pathToStrings(List list){
		Iterator<TreePath> listIt = list.iterator();
		ArrayList returnList = new ArrayList();
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
	
	private String convertToString(Integer methodId){
		HashMap classDefs = classMethods.getClassDefs();
		HashMap methodDefs = classMethods.getMethodDefs();
		HashMap classesToMethods = classMethods.getClassesToMethods();
		Integer classId = findKeyFor(classesToMethods,methodId);
		String className = (String)classDefs.get(classId);
		String methodName = (String)methodDefs.get(methodId);
		return className+"."+methodName;
	}
	
	private Integer findKeyFor(HashMap classesToMethods, Integer method){
		Iterator keyIt = classesToMethods.keySet().iterator();
		while(keyIt.hasNext()){
			Integer nextKey = (Integer)keyIt.next();
			Collection methods = (Collection)classesToMethods.get(nextKey);
			if(methods.contains(method))
				return nextKey;
		}
		return null;
	}
	

}
