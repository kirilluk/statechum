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
	
	private Map<Integer, String> classDefs, methodDefs, methodSignatures;
	private Map<Integer,Integer> objectsToClasses;
	private Map<Integer,Set<Integer>> classesToMethods;
	private File source;
	
	public ClassMethodDefsHandler(File f){
		super();
		source = f;
		classDefs = new HashMap<Integer, String>();
		methodDefs = new HashMap<Integer, String>();
		methodSignatures = new HashMap<Integer, String>();
		classesToMethods = new HashMap<Integer, Set<Integer>>();
		objectsToClasses = new HashMap<Integer, Integer>();
	}
	
	@Override 
	public void startElement(@SuppressWarnings("unused") String uri, @SuppressWarnings("unused") String localName, String qName, Attributes attributes){
		if(qName.equals("classDef")){
			Integer id = Integer.valueOf(attributes.getValue("classId"));
			String name = attributes.getValue("name");
			classDefs.put(id, name);
		}
		else if(qName.equals("methodDef")){
			Integer id = Integer.valueOf(attributes.getValue("methodId"));
			String name = attributes.getValue("name");
			methodDefs.put(id, name);
			methodSignatures.put(id, attributes.getValue("signature"));
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

	public Map<Integer, String> getMethodSignatures() {
		return methodSignatures;
	}

}
