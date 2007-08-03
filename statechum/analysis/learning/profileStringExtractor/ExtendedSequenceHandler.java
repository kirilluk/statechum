package statechum.analysis.learning.profileStringExtractor;

/**
 * Every time an abstract function is observed in a trace, the call-sequence of methods is 
 * recorded as an observation of that function. These observations can be used either by
 * findloops or RPNI to work out the machine that corresponds to the function.
 */

import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.*;

import javax.swing.tree.TreePath;

import org.xml.sax.SAXException;

public class ExtendedSequenceHandler extends SequenceHandler {
	
	protected Map<String,Collection<List<Integer>>> functionsToObservations; 
	
	public ExtendedSequenceHandler(Map<String,List<TreePath>> functions, ClassMethodDefsHandler classMethods){
		super(functions, classMethods);
		functionsToObservations = new HashMap<String,Collection<List<Integer>>>();
	}

	@Override
	protected void checkSequenceForFunction(List<Integer> sequence) {
		for(String key:functions.keySet()){
			List<TreePath> l = functions.get(key);
			List<String> stringL = pathToStrings(l);
			if(containsString(sequence, stringL)){
				functionString.add(key);
				Collection obs = getListFor(key);
				if(!sequence.isEmpty()){
					obs.add(toListOfMethodNames(sequence));
					functionsToObservations.put(key, obs);
				}
				methodSequence.clear();
			}
		}
	}
	
	protected List<String> toListOfMethodNames(List<Integer> methodStack){
		ArrayList<String> returnList = new ArrayList<String>();
		Iterator<Integer> stackIt = methodStack.iterator();
		while(stackIt.hasNext()){
			returnList.add(ticketToString.get(stackIt.next()));
		}
		return returnList;
	}
	
	@Override
	public void endDocument() throws SAXException {
		super.endDocument();
		Iterator<String> keyIt = functionsToObservations.keySet().iterator();
		while(keyIt.hasNext()){
			String key = keyIt.next();
			Collection<List<Integer>> obs = functionsToObservations.get(key);
			try{
				FileOutputStream fos = new FileOutputStream(key);
				OutputStreamWriter out= new OutputStreamWriter(fos, "UTF-8");
				out.write("passive\n");
				Iterator<List<Integer>> obsIt = obs.iterator();
				while(obsIt.hasNext()){
					out.write("+ ");
					List<Integer> sequence = obsIt.next();
					Iterator<Integer> sequenceIt = sequence.iterator();
					while(sequenceIt.hasNext()){
						out.write(sequenceIt.next()+" ");
					}
					out.write("\n");
				}
				out.close();
				fos.close();
			}catch (Exception e){e.printStackTrace();}
			
			
		}
	}

	protected Collection<List<Integer>> getListFor(String key){
		Collection<List<Integer>> observations = functionsToObservations.get(key);
		if(observations == null)
			return new ArrayList<List<Integer>>();
		else
			return observations;
	}
	
	

}
