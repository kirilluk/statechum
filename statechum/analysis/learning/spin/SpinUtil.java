/**
 * To use this, LTL2BA needs to be on the system path, as it is called 
 * by the LTL2BA Java interfaces. 
 * 
 * Future versions will similarly require SPIN to be on the system path as well.
 */

package statechum.analysis.learning.spin;
import java.io.*;
import java.util.*;

import statechum.JUConstants;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

import jltl2ba.*;

public class SpinUtil {
	
	static int functionCounter;
	static String defines;
	static StringWriter sw;
	
	public static boolean check(DirectedSparseGraph g, Set<String> ltl){
		functionCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(g, ltl);
		return true;
	}
	
	public static void generatePromela(DirectedSparseGraph g, Set<String> ltl){
		generateProcsAndInit(g, ltl);
		
		try{
			FileOutputStream fos = new FileOutputStream("promelaMachine");
			OutputStreamWriter out= new OutputStreamWriter(fos, "UTF-8");
			out.write(defines.concat(sw.toString()));
			out.close();
			fos.close();
		}catch (Exception e){e.printStackTrace();}
	}
	
	private static void addLtl(Set<String> ltl, Map<String, Integer> functionMap){
		try{
			for (String string : ltl) {
				StringBuffer output = LowLevel.exec(string);
				sw.write("\n" + output.toString());
			}
			
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}
	
	private static void generateDefines(Map<String, Integer> functionMap){
		
		for (String key : functionMap.keySet()) {
			defines = defines.concat("#define "+key+ "\t"+"(input == "+functionMap.get(key)+")\n");
		}
	}
	
	private static void generateProcsAndInit(DirectedSparseGraph g, Set<String> ltl){
		setup();
		Iterator<DirectedEdge> edgeIt = g.getEdges().iterator();
		HashMap<String,Integer> functionMap = new HashMap<String,Integer>();
		HashMap<String,Integer> stateMap = new HashMap<String,Integer>();
		int  stateCounter = 0;
		while(edgeIt.hasNext()){
			DirectedEdge e = edgeIt.next();
			Set labels = (Set)e.getUserDatum(JUConstants.LABEL);
			Iterator labelIt = labels.iterator();
			String currentState = e.getSource().getUserDatum(JUConstants.LABEL).toString();
			if(!stateMap.keySet().contains(currentState)){
				stateMap.put(currentState, new Integer(stateCounter));
				stateCounter++;
			}
			String toState = e.getDest().getUserDatum(JUConstants.LABEL).toString();
			if(!stateMap.keySet().contains(toState)){
				stateMap.put(toState, new Integer(stateCounter));
				stateCounter++;
			}
			while(labelIt.hasNext()){
				String label = labelIt.next().toString();
				if(!functionMap.keySet().contains(label)){
					functionMap.put(label, new Integer(functionCounter));
					functionCounter++;
				}
				sw.write("\n\t:: ((state=="+stateMap.get(currentState)+") && (input=="+functionMap.get(label)+")) -> state="+stateMap.get(toState)+";\n");
				addSuccessorIf(sw, (DirectedSparseVertex)e.getDest(), functionMap);
			}
			
		}
		sw.write("\n\tod\n}\n");
		sw.write("\ninit {\nrun machine();\n}");
		if(ltl!=null)
			addLtl(ltl, functionMap);
		printLegend(sw, functionMap);
		generateDefines(functionMap);
	}
	
	private static void addSuccessorIf(StringWriter sw, DirectedSparseVertex state, HashMap<String, Integer> functionMap){
		sw.write("\tif");
		Iterator<Edge> outgoingTransitions = state.getOutEdges().iterator();
		while(outgoingTransitions.hasNext()){
			Edge e = outgoingTransitions.next();
			Set<String> labels = (Set<String>) e.getUserDatum(JUConstants.LABEL);
			Iterator<String> labelIt = labels.iterator();
			while(labelIt.hasNext()){
				String label = labelIt.next();
				if(!functionMap.keySet().contains(label)){
					functionMap.put(label, new Integer(functionCounter));
					functionCounter++;
				}
				sw.write("\n\t\t:: input = "+functionMap.get(label)+";" );
			}
		}
		if(state.getOutEdges().isEmpty())
			sw.write("\n\t\t::break");
		sw.write("\n\tfi");
	}
	
	private static void printLegend(StringWriter sw, HashMap<String, Integer> functionMap){
		Iterator<String> keyIt = functionMap.keySet().iterator();
		sw.write("\n/*");
		while(keyIt.hasNext()){
			String key = keyIt.next();
			sw.write("\n"+key+" - "+ functionMap.get(key));
		}
		sw.write("\n*/");
	}
	
	private static void setup(){
		sw = new StringWriter();
		sw.write("int state=0;\nint input;\nproctype machine(){\n\tdo");
	}

}
