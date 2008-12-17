/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */ 
package statechum.analysis.learning.spin;

import java.io.*;
import java.util.*;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.util.*;
import statechum.analysis.learning.rpnicore.*;

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

	static String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"promelaMachine";
	
	static StringWriter sw;

	static Map<String, Integer> functionMap;

	static Map<Integer, String> inverseFunctionMap;
	
	public static SpinResult check(LearnerGraph temp, LearnerGraph current, Collection<String> ltl) {
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(temp.pathroutines.getGraph());
		createInverseMap();
		SpinResult sr = checkLTL(concatenatedLTL(ltl));
		sr.ensureCountersConsistent(current);
		return sr;
	}
	
	public static SpinResult check(LearnerGraph temp, Collection<String> ltl) {
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(temp.pathroutines.getGraph());
		createInverseMap();
		SpinResult sr = checkLTL(concatenatedLTL(ltl));
		//List<List<String>>sortedCounters = sort(counters);
		return sr;
	}
	
	static void removeInvalidPrefixCounters(List<List<String>> counters, LearnerGraph current){
		Iterator<List<String>> counterIt = counters.iterator();
		Collection<List<String>> toBeRemoved = new HashSet<List<String>>();
		LearnerGraph counterPTA = new LearnerGraph(Configuration.getDefaultConfiguration());
		while(counterIt.hasNext()){
			List<String> counter = counterIt.next();
			if(current.paths.tracePath(counter.subList(0, counter.size()-1))!=AbstractOracle.USER_ACCEPTED)
				toBeRemoved.add(counter);
			else if(current !=null)
				if(current.paths.tracePath(counter) == AbstractOracle.USER_ACCEPTED)
					toBeRemoved.add(counter);
			
			else{ 
				try{
					counterPTA.paths.augmentPTA(counter, false, false,null);
				}
				catch(Exception e){
					toBeRemoved.add(counter);
				}
			}
				
		}
		counters.removeAll(toBeRemoved);
	}
	
	public static List<List<String>> sort(Set<List<String>> counters){
		ArrayList<List<String>> counterList = new ArrayList<List<String>>();
		counterList.addAll(counters);
		Collections.sort(counterList, new Comparator<List<String>>() {
            public int compare(List<String> s1, List<String> s2) {
                 if (s1.size() < s2.size()) {
                    return -1;
                }
                else if (s1.size() > s2.size()) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
        });
		return counterList;
	}
	
	private static String concatenatedLTL(Collection<String> ltl){
		String ltlString = "";
		for (String string : ltl) {
			if(!ltlString.equals(""))
				ltlString = ltlString.concat(" || "+string);
			else
				ltlString = ltlString.concat(string);
		}
		return ltlString;
	}
	
	public static int check (List<String> question, Collection<String> ltl){
		functionCounter = 0;
		stateCounter = 0;
		sw = new StringWriter();
		defines = new String();
		generatePromela(question);
		createInverseMap();
		SpinResult sr = checkLTL(concatenatedLTL(ltl));
		if(sr.isPass())
			return -1;
		else if(sr.getCounters().size()>=1)
			return sr.getCounters().iterator().next().size();
		else
			return 0;
	}
	
	
	
	private static SpinResult checkLTL(String ltl){
		addLtl(ltl);
		generateDefines(functionMap);
		File promelaMachine  = new File(fileRef);
		OutputUtil.write(defines.concat(sw.toString()), promelaMachine);
		return runSpin('s');
	}

	private static void createInverseMap() {
		inverseFunctionMap = new HashMap<Integer, String>();
		for (String key : functionMap.keySet()) {
			inverseFunctionMap.put(functionMap.get(key), key);
		}
	}

	/*
	 * returns a set of counter examples
	 */
	private static SpinResult runSpin(char safetyLiveness) {
		SpinResult sr = new SpinResult();
		List<String[]> cmdArray = new ArrayList<String[]>();
		cmdArray.add(0, (String[]) Arrays.asList("rm", "*.trail").toArray());
		cmdArray.add(1, (String[]) Arrays
				.asList("spin", "-Z", "promelaMachine").toArray());
		cmdArray.add(2, (String[]) Arrays.asList("spin", "-a", "-X",
				"promelaMachine").toArray());
		if( safetyLiveness == 's'){ //compile pan for checking safety properties
			cmdArray.add(3, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
				"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE",  "-DSAFETY", "-DNXT", "-DBFS", "-DREACH",
				//"-DNOREDUCE", 
				"-DNOFAIR", "pan.c").toArray());
			cmdArray.add(4, (String[]) Arrays.asList(new File(fileRef).getParentFile().getAbsolutePath()+System.getProperty("file.separator")+"pan", "-v", "-X",
				"-m10000", "-w19", "-A", "-e", "-c0").toArray()); //"-i"
		}
		else{ //compile pan for checking liveness properties
			cmdArray.add(3, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
					"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE", "-DNXT",
					//"-DNOREDUCE", 
					"-DNOFAIR", "pan.c").toArray());
			cmdArray.add(4, (String[]) Arrays.asList(new File(fileRef).getParentFile().getAbsolutePath()+System.getProperty("file.separator")+"pan", "-v", "-X",
					"-m10000", "-w19", "-a", "-c1").toArray());
		}
		for (int i = 0; i < 5; i++) {
			try {
				String line;
				ProcessBuilder pb = new ProcessBuilder(cmdArray.get(i));
				pb.directory(new File(fileRef).getParentFile());
				Process proc = pb.start();
				InputStreamReader tempReader = new InputStreamReader(
	                new BufferedInputStream(proc.getInputStream()));
	            BufferedReader reader = new BufferedReader(tempReader); 
	            
	            while ((line = reader.readLine()) != null) {
	            	if(line.contains("errors: 0")){
	            		sr.setCounters(new ArrayList<List<String>>());
	            		sr.setPass(true);
	            		return sr;
	            	}
	            	//System.out.println(line);
					
				}
			} catch (Throwable e) {
				e.printStackTrace();
			}
			
		}
		sr.setPass(false);
		sr.setCounters(getCounterExamples());
		return sr;
	}
	
	

	private static List<List<String>> getCounterExamples(){
		Set<List<String>> counterExamples = new HashSet<List<String>>();
		String[] filelist;
		File f = new File(fileRef).getParentFile();		
		SpinUtil sp = new SpinUtil();
		filelist = f.list(sp.new TrailFileFilter());
		for(int i=0; i< filelist.length; i++){
			List<String> counterExample = getCounterExample(i);
			if(!counterExample.isEmpty())
				counterExamples.add(counterExample);
		}
		return sort(counterExamples);
	}
	
	public class TrailFileFilter implements FilenameFilter 
	{ 
	   public boolean accept(@SuppressWarnings("unused") java.io.File f, java.lang.String g) 
	    { 
	       return (g.indexOf(".trail") != -1);
	   } 
	}

	
	private static int numAcceptingSuccessors(DirectedSparseVertex v){
		int succs = 0;
		Iterator<DirectedEdge> outgoingIt = v.getOutEdges().iterator();
		while(outgoingIt.hasNext()){
			DirectedEdge e = outgoingIt.next();
			if(DeterministicDirectedSparseGraph.isAccept(e.getDest()))
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
			
			String currentState = v.getUserDatum(JUConstants.LABEL).toString();
			if(v.containsUserDatumKey(JUConstants.INITIAL))
				currentState = "Init";
			if (!stateMap.keySet().contains(currentState)) {
				stateMap.put(currentState, new Integer(stateCounter));
				stateCounter++;
			}
			if (!DeterministicDirectedSparseGraph.isAccept(v)){
				sw.write(currentState + ": false;\n");
				continue;
			}
			if(numAcceptingSuccessors(v)==0){
				sw.write(currentState + ": goto end;\n"); //input=50000; 
				continue;
			}
			else{
				sw.write(currentState+":\n"+"\tif");
				Iterator<DirectedEdge> outEdges = v.getOutEdges().iterator();
				while(outEdges.hasNext()){
					DirectedEdge e = outEdges.next();
					Set<String> labels = (Set<String>)e.getUserDatum(JUConstants.LABEL);
					
					if(!DeterministicDirectedSparseGraph.isAccept(e.getDest()))
							continue;
					String toState = e.getDest().getUserDatum(JUConstants.LABEL).toString();
					if(e.getDest().containsUserDatumKey(JUConstants.INITIAL))
						toState = "Init";
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
		
		sw.write("end: \n\tskip;\n}\n\ninit {\nrun machine();\n}");
		printLegend(sw, functionMap);
	}
	
	private static void generatePromela(List<String> question) {
		functionMap = new HashMap<String, Integer>();
		sw = new StringWriter();
		sw.write("int input=50000;\nproctype machine(){\n");
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
	
	private static List<String> getCounterExample(int i){
		List<String> counterExample = new ArrayList<String>();
		String[] trace = new String[]{"spin", "-t"+(i+1), "-p", "-c", "promelaMachine"};
		LinkedList<String> SpinData = new LinkedList<String>(); 
		try {
			String line;
			ProcessBuilder pb = new ProcessBuilder(trace);
			pb.directory(new File(fileRef).getParentFile());
			
			Process proc = pb.start();
			BufferedReader input = new BufferedReader(new InputStreamReader(
					proc.getInputStream()));
			while ((line = input.readLine()) != null) {
				SpinData.add(line);
				//System.out.println(line);
				if(line.contains("trail ends after"))
					break;
				if (line.contains("<valid end state>")&&line.contains("proc  1")){
					return new ArrayList<String>(); //don't want to return this counterexample
				}
				else if (line.contains("[input")) {
					int inputIndex = line.indexOf("[input =") + 8;
					int closingBracket = line.indexOf("]", inputIndex);
					String function = inverseFunctionMap.get(Integer
							.valueOf(line.substring(inputIndex,
									closingBracket).trim()));
					if(function !=null)
						counterExample.add(function);
				} 
				else if (line.contains("<<<<<"))
					break;

			}
			proc.destroy();
		} catch (Throwable e) {
			e.printStackTrace();
		}
		
		if (counterExample.isEmpty())
			return counterExample;
		else{
			counterExample.remove(counterExample.size()-1);
			return counterExample;
		}
	}

	private static void setup(DirectedSparseGraph g,
			Map<String, Integer> stateMap, Map<String, Integer> functionMap) {
		sw = new StringWriter();
		DirectedSparseVertex v = (DirectedSparseVertex) DeterministicDirectedSparseGraph
				.findInitial(g);
		String state = v.getUserDatum(JUConstants.LABEL).toString();
		if (!stateMap.keySet().contains(state)) {
			stateMap.put(state, new Integer(stateCounter));
			stateCounter++;
		}
		sw.write("int input=50000;\nproctype machine(){\ngoto Init;\n");
	}

}
