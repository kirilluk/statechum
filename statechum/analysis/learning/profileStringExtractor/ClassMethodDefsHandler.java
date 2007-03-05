package statechum.analysis.learning.profileStringExtractor;

import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.Attributes;
import java.util.*;
import java.io.*;

public class ClassMethodDefsHandler extends DefaultHandler implements Serializable{
	
	HashMap classDefs, methodDefs, classesToMethods, objectsToClasses;
	File source;
	
	public ClassMethodDefsHandler(File f){
		super();
		source = f;
		classDefs = new HashMap();
		methodDefs = new HashMap();
		classesToMethods = new HashMap();
		objectsToClasses = new HashMap();
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
				Collection c = (Collection)classesToMethods.get(classId);
				c.add(id);
				classesToMethods.put(classId, c);
			}
			else{
				HashSet methods = new HashSet();
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

	public HashMap getClassDefs() {
		return classDefs;
	}

	public HashMap getMethodDefs() {
		return methodDefs;
	}

	public HashMap getClassesToMethods() {
		return classesToMethods;
	}
	
	public HashMap getObjectsToClasses(){
		return objectsToClasses;
	}

	public File getSource() {
		return source;
	}

}
