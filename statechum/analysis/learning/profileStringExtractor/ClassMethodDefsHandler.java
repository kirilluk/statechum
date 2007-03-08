package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import java.util.*;
import java.io.*;

/** Loads classes/methods from an xml file saved by the profiling framework. */
public class ClassMethodDefsHandler extends DefaultHandler implements Serializable{
	
	/**
	 * The version ID for serialization.
	 */
	private static final long serialVersionUID = 2167687392681999116L;
	
	Map<Integer, String> classDefs, methodDefs;
	Map<Integer,Integer> objectsToClasses;
	Map<Integer,Set<Integer>> classesToMethods;
	File source;
	
	public ClassMethodDefsHandler(File f){
		super();
		source = f;
		classDefs = new HashMap<Integer, String>();
		methodDefs = new HashMap<Integer, String>();
		classesToMethods = new HashMap<Integer, Set<Integer>>();
		objectsToClasses = new HashMap<Integer, Integer>();
	}
	
	public void startElement(String uri, String localName, String qName, Attributes attributes){
		if(qName.equals("classDef")){
			Integer id = Integer.valueOf(attributes.getValue("classId"));
			String name = attributes.getValue("name");
			classDefs.put(id, name);
		}
		else if(qName.equals("methodDef")){
			Integer id = Integer.valueOf(attributes.getValue("methodId"));
			String name = attributes.getValue("name");
			methodDefs.put(id, name);
			Integer classId = Integer.valueOf(attributes.getValue("classIdRef"));
			if(classesToMethods.get(classId)!=null){
				Set<Integer> c = classesToMethods.get(classId);
				c.add(id);
				classesToMethods.put(classId, c);
			}
			else{
				HashSet<Integer> methods = new HashSet<Integer>();
				methods.add(id);
				classesToMethods.put(classId, methods);
			}
		}
		else if(qName.equals("objAlloc")){
			Integer classId = Integer.valueOf(attributes.getValue("classIdRef"));
			Integer objectId = Integer.valueOf(attributes.getValue("objId"));
			objectsToClasses.put(objectId, classId);
		}
	}

	public Map<Integer, String> getClassDefs() {
		return classDefs;
	}

	public Map<Integer, String> getMethodDefs() {
		return methodDefs;
	}

	public Map<Integer, Set<Integer>> getClassesToMethods() {
		return classesToMethods;
	}
	
	public Map<Integer, Integer> getObjectsToClasses(){
		return objectsToClasses;
	}

	public File getSource() {
		return source;
	}

}
