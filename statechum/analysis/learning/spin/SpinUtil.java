
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




package statechum.analysis.learning.spin;

import java.io.*;
import java.util.*;

import statechum.JUConstants;
import statechum.analysis.learning.RPNIBlueFringeLearner;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

import jltl2ba.*;

/**
 * @author nw
 *
 */
public class SpinUtil {
	
	// To use this, LTL2BA and SPIN need to be on the system path. 


	static int functionCounter, stateCounter;

	static String defines;

	static String fileRef = "temp"+System.getProperty("file.separator")+"promelaMachine";
	
	static StringWriter sw;

	static Map<String, Integer> functionMap;

	static Map<Integer, String> inverseFunctionMap;

	public static boolean check(DirectedSparseGraph g, Set<String> ltl) {
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(g);
		createInverseMap();
		for (String string : ltl) {
			if(!checkLTL(string))
				return false;
		}
		return true;
	}
	
	public static boolean check (List<String> question, Set<String> ltl){
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(question);
		createInverseMap();
		for (String string : ltl) {
			if(!checkLTL(string))
				return false;
		}
		return true;
	}
	
	private static void write(StringWriter writer, File f){
		try {
			f.getParentFile().mkdirs();
			f.createNewFile();
			FileOutputStream fos = new FileOutputStream(f);
			OutputStreamWriter out = new OutputStreamWriter(fos, "UTF-8");
			out.write(defines.concat(writer.toString()));
			out.close();
			fos.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private static boolean checkLTL(String ltl){
		addLtl(ltl.substring(1));
		generateDefines(functionMap);
		File promelaMachine  = new File(fileRef);
		write(sw, promelaMachine);
		return runSpin(ltl.charAt(0));
	}

	private static void createInverseMap() {
		inverseFunctionMap = new HashMap<Integer, String>();
		for (String key : functionMap.keySet()) {
			inverseFunctionMap.put(functionMap.get(key), key);
		}
	}

	private static boolean runSpin(char safetyLiveness) {
		List<String[]> cmdArray = new ArrayList<String[]>();
		cmdArray.add(0, (String[]) Arrays
				.asList("spin", "-Z", "promelaMachine").toArray());
		cmdArray.add(1, (String[]) Arrays.asList("spin", "-a", "-X",
				"promelaMachine").toArray());
		if( safetyLiveness == 's'){ //compile pan for checking safety properties
			cmdArray.add(2, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
				"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE",  "-DBFS", "-DSAFETY", "-DNXT",
				"-DNOREDUCE", "-DNOFAIR", "pan.c").toArray());
			cmdArray.add(3, (String[]) Arrays.asList(new File(fileRef).getParentFile().getAbsolutePath()+System.getProperty("file.separator")+"pan", "-v", "-X",
				"-m10000", "-w19", "-A", "-i", "-c1").toArray());
		}
		else{ //compile pan for checking liveness properties
			cmdArray.add(2, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
					"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE", "-DNXT",
					"-DNOREDUCE", "-DNOFAIR", "pan.c").toArray());
			cmdArray.add(3, (String[]) Arrays.asList(new File(fileRef).getParentFile().getAbsolutePath()+System.getProperty("file.separator")+"pan", "-v", "-X",
					"-m10000", "-w19", "-a", "-c1").toArray());
		}
		for (int i = 0; i < 4; i++) {
			try {
				String line;
				ProcessBuilder pb = new ProcessBuilder(cmdArray.get(i));
				pb.directory(new File(fileRef).getParentFile());
				
				Process proc = pb.start();
				InputStreamReader tempReader = new InputStreamReader(
		                new BufferedInputStream(proc.getInputStream()));
		            BufferedReader reader = new BufferedReader(tempReader);
				while ((line = reader.readLine()) != null) {
					//System.out.println(line);
					 if(line.contains("errors: 0"))
						 return true;
				}
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}
		return false;
	}

	
	private static int numAcceptingSuccessors(DirectedSparseVertex v){
		int succs = 0;
		Iterator<DirectedEdge> outgoingIt = v.getOutEdges().iterator();
		while(outgoingIt.hasNext()){
			DirectedEdge e = outgoingIt.next();
			if(RPNIBlueFringeLearner.isAccept(e.getDest()))
				succs++;
		}
		return succs;
	}
	
	private static void generatePromela(DirectedSparseGraph g) {
		HashMap<String, Integer> stateMap = new HashMap<String, Integer>();
		functionMap = new HashMap<String, Integer>();
		setup(g, stateMap, functionMap);
		Iterator<DirectedSparseVertex> stateIt = g.getVertices().iterator();
		
		while (stateIt.hasNext()) {
			DirectedSparseVertex v = stateIt.next();
			if (!RPNIBlueFringeLearner.isAccept(v))
				continue;
			String currentState = v.getUserDatum(JUConstants.LABEL).toString();
			if (!stateMap.keySet().contains(currentState)) {
				stateMap.put(currentState, new Integer(stateCounter));
				stateCounter++;
			}
			if(numAcceptingSuccessors(v)==0){
				sw.write(v + ": false;\n");
				continue;
			}
			else{
				sw.write(v+":\n"+"\tif");
				Iterator<DirectedEdge> outEdges = v.getOutEdges().iterator();
				while(outEdges.hasNext()){
					DirectedEdge e = outEdges.next();
					Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
					
					if(!RPNIBlueFringeLearner.isAccept(e.getDest()))
							continue;
					String toState = e.getDest().getUserDatum(JUConstants.LABEL).toString();
					if (!stateMap.keySet().contains(toState)) {
						stateMap.put(toState, new Integer(stateCounter));
						stateCounter++;
					}
					Iterator labelIt = labels.iterator();
					while (labelIt.hasNext()) {
						String label = labelIt.next().toString();
						if (!functionMap.keySet().contains(label)) {
							functionMap.put(label, new Integer(functionCounter));
							functionCounter++;
						}
						sw.write("\n\t:: input=" + functionMap.get(label)
								+ " -> goto " + toState + ";\n");
					}
				}
				sw.write("\tfi;\n");
			}
		}
		
		sw.write("}\n\ninit {\nrun machine();\n}");
		printLegend(sw, functionMap);
		generateDefines(functionMap);
	}
	
	private static void generatePromela(List<String> question) {
		functionMap = new HashMap<String, Integer>();
		sw = new StringWriter();
		sw.write("int input = 50000;\nproctype machine(){\n");
		Iterator<String> questionIt = question.iterator();
		
		while (questionIt.hasNext()) {
			String symb = questionIt.next();
			if (!functionMap.keySet().contains(symb)) {
				functionMap.put(symb, new Integer(functionCounter));
				functionCounter++;
			}
			sw.write("input="+functionMap.get(symb)+";\n");
			
		}
		
		sw.write("}\n\ninit {\nrun machine();\n}");
		printLegend(sw, functionMap);
		
	}

	private static void addLtl(String ltl) {
		try {
			ensureApplicable(ltl);
			StringBuffer output = LowLevel.exec(ltl);
			sw.write("\n" + output.toString());
		} catch (Exception e) {e.printStackTrace();}
	}
	
	private static void ensureApplicable(String ltl){
		String[] splitString = ltl.split("\\p{Punct}|X|U|V");
		for (String string : splitString) {
			if(string.trim().equals("")||string.trim().equals("[]"))
				continue;
			else if(!functionMap.keySet().contains(string)){
				functionMap.put(string, new Integer(functionCounter));
				functionCounter++;
			}
		}
	}

	private static void generateDefines(Map<String, Integer> functionMap) {

		for (String key : functionMap.keySet()) {
			defines = defines.concat("#define " + key + "\t" + "(input == "
					+ functionMap.get(key) + ")\n");
		}
	}


	private static void printLegend(StringWriter sw,
			Map<String, Integer> functionMap) {
		Iterator<String> keyIt = functionMap.keySet().iterator();
		sw.write("\n/*");
		while (keyIt.hasNext()) {
			String key = keyIt.next();
			sw.write("\n" + key + " - " + functionMap.get(key));
		}
		sw.write("\n*/");
	}

	public static List<String> getCurrentCounterExample() {
		List<String> counterExample = new ArrayList<String>();
		String[] trace = (String[]) Arrays.asList("spin", "-t", "-p",
				"promelaMachine").toArray();
		try {
			String line;
			ProcessBuilder pb = new ProcessBuilder(trace);
			pb.directory(new File(fileRef).getParentFile());
			
			Process proc = pb.start();
			BufferedReader input = new BufferedReader(new InputStreamReader(
					proc.getInputStream()));
			while ((line = input.readLine()) != null) {
				if (line.contains("[input")) {
					int inputIndex = line.indexOf("[input =") + 8;
					int closingBracket = line.indexOf("]", inputIndex);
					counterExample.add(inverseFunctionMap.get(Integer
							.valueOf((String) line.substring(inputIndex,
									closingBracket).trim())));
				} else if (line.contains("<<<<<"))
					break;

			}
		} catch (Throwable e) {
			e.printStackTrace();
		}
		return counterExample;
	}

	private static void setup(DirectedSparseGraph g,
			Map<String, Integer> stateMap, Map<String, Integer> functionMap) {
		sw = new StringWriter();
		DirectedSparseVertex v = (DirectedSparseVertex) RPNIBlueFringeLearner
				.findInitial(g);
		String state = v.getUserDatum(JUConstants.LABEL).toString();
		if (!stateMap.keySet().contains(state)) {
			stateMap.put(state, new Integer(stateCounter));
			stateCounter++;
		}
		sw.write("int input = 50000;\nproctype machine(){\ngoto Init;\n");
	}

}
