package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import java.util.*;

import javax.swing.tree.TreePath;

public abstract class AbstractHandler extends DefaultHandler {
	
	protected Map<Integer,String> ticketToString;
	protected Map<String,List<TreePath>> functions;
	protected Set<Integer> doneTickets;
	protected ClassMethodDefsHandler classMethods;
	protected List<Integer> methodSequence;
	protected List<String> functionString;
	
	public AbstractHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods){
		this.functionString = new ArrayList<String>();
		this.classMethods = classMethods;
		this.functions = functions;
		this.ticketToString = new HashMap<Integer,String>();
		this.methodSequence = new ArrayList<Integer>();
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
	
	public abstract void startElement(String uri, String localName, String qName, Attributes attributes);
	
	protected abstract void checkStackForFunction(List<Integer> sequence);
	
	protected boolean containsString(List<Integer> sequence, List<String> list){
		for(int index=0;index<sequence.size();index++){
			Integer ticket = sequence.get(index);
			if(doneTickets.contains(ticket))
				continue;
			String methodString = ticketToString.get(ticket);
			if(methodString.equals(list.get(0).trim())){
				if(list.size() == 1){
					doneTickets.addAll(sequence);
					return true;
				}
				else
					return containsString(sequence.subList(index+1, sequence.size()), list.subList(1, list.size()));
			}
			
		}
		return false;
	}
	
	/** Given a list of paths, this function returns a list of textual representations of those paths. */
	protected static List<String> pathToStrings(List<TreePath> list){
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
	protected String convertToString(Integer methodId){
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
