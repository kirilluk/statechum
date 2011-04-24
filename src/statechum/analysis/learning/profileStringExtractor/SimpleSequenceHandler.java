package statechum.analysis.learning.profileStringExtractor;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class SimpleSequenceHandler extends DefaultHandler{
	
	private File source;
	private HashSet<Integer> threads;
	private HashMap<Integer, String> classDefs;
	private HashMap<Integer, String> methodDefs;
	private HashMap<Integer, String> methodSignatures;
	private HashMap<Integer, Set<Integer>> classesToMethods;
	private HashMap<Integer, Integer> objectsToClasses;
	protected List<Integer> methodSequence;
	protected Map<Integer,String> ticketToString;
	protected String threadName;

	public SimpleSequenceHandler(File f, String thread){
		super();
		threadName = thread;
		source = f;
		threads = new HashSet<Integer>();
		classDefs = new HashMap<Integer, String>();
		methodDefs = new HashMap<Integer, String>();
		methodSignatures = new HashMap<Integer, String>();
		classesToMethods = new HashMap<Integer, Set<Integer>>();
		objectsToClasses = new HashMap<Integer, Integer>();
		this.methodSequence = new ArrayList<Integer>();
		this.ticketToString = new HashMap<Integer,String>();
	}
	
	/** Every method has a unique id, this function finds a class corresponding 
	 * to this method and returns a full name of this method. 
	 */
	protected String convertToString(Integer methodId){
		Integer classId = findKeyFor(classesToMethods,methodId);
		String className = classDefs.get(classId);
		String methodName = methodDefs.get(methodId);
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
	
	protected String convertToSignatureString(Integer methodId){
		Integer classId = findKeyFor(classesToMethods,methodId);
		String className = classDefs.get(classId);
		String methodName = methodDefs.get(methodId);
		String methodSignature = methodSignatures.get(methodId);
		return className+"."+methodName+methodSignature;
	}

	@Override
	public void startElement(String uri, String localName, String qName,
			Attributes attributes) {
		
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
		else if(qName.equals("methodEntry")){
			String methodIdRef = attributes.getValue("methodIdRef");
			String classIdRef = attributes.getValue("classIdRef");
			String ticketRef = attributes.getValue("ticket");
			Integer thread = Integer.valueOf(attributes.getValue("threadIdRef"));
			if(!(methodIdRef == null) && !(classIdRef == null)&& !(ticketRef == null) && 
					threads.contains(thread)){
				Integer methodId = Integer.valueOf(methodIdRef);
				Integer ticket = Integer.valueOf(ticketRef);
				String methodString = convertToString(methodId);
				methodSequence.add(ticket);
				ticketToString.put(ticket, convertToSignatureString(methodId));
			}
		}
		else if(qName.equals("methodExit")){
			String ticketRef = attributes.getValue("ticket");
			Integer thread = Integer.valueOf(attributes.getValue("threadIdRef"));
			if(!(ticketRef == null)&& threads.contains(thread)){
				Integer ticket = Integer.valueOf(ticketRef);
				//methodSequence.add(ticket);
			}
		}
		else if(qName.equals("threadStart")){
			String groupName = attributes.getValue("groupName");
			if(groupName.equals(this.threadName)){
				Integer number = Integer.valueOf(attributes.getValue("threadId"));
				threads.add(number);
			}
		}
	}
	
	public void endDocument() throws SAXException {
		super.endDocument();
			try{
				FileOutputStream fos = new FileOutputStream(statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"trace.txt");
				OutputStreamWriter out= new OutputStreamWriter(fos, "UTF-8");
				Iterator<Integer> obsIt = methodSequence.iterator();
				out.write("+ ");
				while(obsIt.hasNext()){
					out.write(ticketToString.get(obsIt.next()) + " ");	
				}
				out.close();
				fos.close();
			}catch (Exception e){e.printStackTrace();}
			
			
		}
}
