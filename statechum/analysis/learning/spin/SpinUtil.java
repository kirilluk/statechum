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
import statechum.analysis.learning.RPNIBlueFringeLearner;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

import jltl2ba.*;

public class SpinUtil {
	
	static int functionCounter, stateCounter;
	static String defines;
	static StringWriter sw;
	static Map<String,Integer> functionMap;
	static Map<Integer, String> inverseFunctionMap;
	
	public static boolean check(DirectedSparseGraph g, Set<String> ltl){
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(g, ltl);
		createInverseMap();
		return runSpin();
	}
	
	private static void createInverseMap(){
		inverseFunctionMap = new  HashMap<Integer,String>();
		for (String key : functionMap.keySet()) {
			inverseFunctionMap.put(functionMap.get(key), key);
		}
	}
	
	private static boolean runSpin(){
		boolean pass = false;
		List<String[]> cmdArray = new ArrayList<String[]>();
		cmdArray.add(0, (String[])Arrays.asList("spin", "-Z", "promelaMachine").toArray());
		cmdArray.add(1,(String[])Arrays.asList("spin", "-a", "-X", "promelaMachine").toArray());
		cmdArray.add(2,(String[])Arrays.asList("gcc", "-w", "-o", "pan", "-D_POSIX_SOURCE", "-DMEMLIM=128",  "-DXUSAFE", "-DNXT","-DNOREDUCE",  "-DNOFAIR",  "pan.c").toArray());
		cmdArray.add(3,(String[])Arrays.asList("./pan", "-v", "-X", "-m10000", "-w19",  "-a", "-c1").toArray());
		Runtime rt = Runtime.getRuntime();
		for (int i=0;i<4;i++) {
			try{
				String line;
				Process proc = rt.exec(cmdArray.get(i));
				BufferedReader input = new BufferedReader(new InputStreamReader(proc.getInputStream()));
				while ((line = input.readLine()) != null) {
			    	//if(line.contains("errors: 0"))
			    		//return true;
					System.out.println(line);
			    }
			}catch (Throwable e){
				e.printStackTrace();
			}
		}
		return pass;
	}
	
	private static void generatePromela(DirectedSparseGraph g, Set<String> ltl){
		generateProcsAndInit(g, ltl);
		
		try{
			FileOutputStream fos = new FileOutputStream("promelaMachine");
			OutputStreamWriter out= new OutputStreamWriter(fos, "UTF-8");
			out.write(defines.concat(sw.toString()));
			out.close();
			fos.close();
		}catch (Exception e){e.printStackTrace();}
	}
	
	private static void addLtl(Set<String> ltl){
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
		HashMap<String,Integer> stateMap = new HashMap<String,Integer>();
		functionMap = new HashMap<String,Integer>();
		setup(g, stateMap, functionMap);
		Iterator<DirectedEdge> edgeIt = g.getEdges().iterator();
		
		
		while(edgeIt.hasNext()){
			DirectedEdge e = edgeIt.next();
			if(!RPNIBlueFringeLearner.isAccept(e.getDest()))
				continue;
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
		sw.write("\n\tfi\n\tod\n}\n");
		sw.write("\ninit {\nrun machine();\n}");
		if(ltl!=null)
			addLtl(ltl);
		printLegend(sw, functionMap);
		generateDefines(functionMap);
	}
	
	private static void addSuccessorIf(StringWriter sw, DirectedSparseVertex state, Map<String, Integer> functionMap){
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
	
	private static void printLegend(StringWriter sw, Map<String, Integer> functionMap){
		Iterator<String> keyIt = functionMap.keySet().iterator();
		sw.write("\n/*");
		while(keyIt.hasNext()){
			String key = keyIt.next();
			sw.write("\n"+key+" - "+ functionMap.get(key));
		}
		sw.write("\n*/");
	}
	
	public static List<String> getCurrentCounterExample(){
		List<String> counterExample = new ArrayList<String>();
		String[] trace = (String[])Arrays.asList("spin", "-t","-p","promelaMachine").toArray();
		Runtime rt = Runtime.getRuntime();
		try{
			String line;
			Process proc = rt.exec(trace);
			BufferedReader input = new BufferedReader(new InputStreamReader(proc.getInputStream()));
			while ((line = input.readLine()) != null) {
		    	if(line.contains("[input")){
		    		int inputIndex = line.indexOf("[input =")+8;
		    		int closingBracket = line.indexOf("]", inputIndex);
		    		counterExample.add(inverseFunctionMap.get(Integer.valueOf((String)line.substring(inputIndex, closingBracket).trim())));
		    	}
		    	else if(line.contains("<<<<<"))
		    		break;
		  
		    }
		}catch (Throwable e){
			e.printStackTrace();
		}
		return counterExample;
	}
	
	private static void setup(DirectedSparseGraph g, Map<String,Integer> stateMap, Map<String,Integer> functionMap){
		sw = new StringWriter();
		DirectedSparseVertex v = (DirectedSparseVertex)RPNIBlueFringeLearner.findInitial(g);
		String state = v.getUserDatum(JUConstants.LABEL).toString();
		if(!stateMap.keySet().contains(state)){
			stateMap.put(state, new Integer(stateCounter));
			stateCounter++;
		}
		sw.write("int state;\nint input;\nproctype machine(){\n\tstate=0;"+getInputBranch(v, functionMap)+"\n\tdo\n\t:: if");
	}
	
	private static String getInputBranch(DirectedSparseVertex init, Map<String,Integer> functionMap){
		String ret = "\n\tif";
		Set<DirectedSparseEdge> outEdges = init.getOutEdges();
		for (DirectedSparseEdge edge : outEdges) {
			Set<String> inputs = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
			if(!RPNIBlueFringeLearner.isAccept(edge.getDest()))
				continue;
			for (String string : inputs) {
				if(!functionMap.keySet().contains(string)){
					functionMap.put(string, new Integer(functionCounter));
					functionCounter++;
				}
				ret = ret.concat("\n\t\t::input="+functionMap.get(string));
			}
		}
		ret = ret.concat("\n\tfi;");
		
		return ret;
	}

}
