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
import statechum.Helper;
import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.util.*;
import statechum.analysis.learning.rpnicore.*;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;

import jltl2ba.*;

/** To use this, either LTL2BA and SPIN need to be on the system path or LTL2BA has to be set 
 * such as by passing -DLTL2BA=/usr/local/bin/ltl2ba as an argument to VM. 
 * 
 * @author nw
 *
 */
public class SpinUtil {
	
	int functionCounter=0, stateCounter=0;

	String defines;

	String tempDir = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP);
	//static String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"promelaMachine";
	
	final StringWriter sw = new StringWriter();

	Map<String, Integer> functionMap;

	Map<Integer, String> inverseFunctionMap;
	
	final Configuration config;
	final ConvertALabel converter;
	
	public SpinUtil(Configuration c, ConvertALabel conv)
	{
		config = c;converter = conv;
	}
	
	public SpinResult check(LearnerGraph temp, LearnerGraph current, Collection<String> ltl) {
		defines = new String();
		generatePromela(temp.pathroutines.getGraph());
		createInverseMap();
		SpinResult sr = checkLTL(concatenatedLTL(ltl));
		sr.ensureCountersConsistent(current);
		return sr;
	}
	
	public SpinResult check(LearnerGraph temp, Collection<String> ltl) {
		functionCounter = 0;
		stateCounter = 0;
		defines = new String();
		generatePromela(temp.pathroutines.getGraph());
		createInverseMap();
		SpinResult sr = checkLTL(concatenatedLTL(ltl));
		//List<List<String>>sortedCounters = sort(counters);
		return sr;
	}
	
	static void removeInvalidPrefixCounters(List<List<Label>> counters, LearnerGraph current){
		Iterator<List<Label>> counterIt = counters.iterator();
		Collection<List<Label>> toBeRemoved = new HashSet<List<Label>>();
		LearnerGraph counterPTA = new LearnerGraph(Configuration.getDefaultConfiguration());
		while(counterIt.hasNext()){
			List<Label> counter = counterIt.next();
			if(current.paths.tracePathPrefixClosed(counter.subList(0, counter.size()-1))!=AbstractOracle.USER_ACCEPTED)
				toBeRemoved.add(counter);
			else if(current !=null)
				if(current.paths.tracePathPrefixClosed(counter) == AbstractOracle.USER_ACCEPTED)
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
	
	public static List<List<Label>> sort(Set<List<Label>> counters){
		ArrayList<List<Label>> counterList = new ArrayList<List<Label>>();
		counterList.addAll(counters);
		Collections.sort(counterList, new Comparator<List<Label>>() {
            public @Override int compare(List<Label> s1, List<Label> s2) {
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
	
	public int check (List<Label> question, Collection<String> ltl){
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
	
	
	
	private SpinResult checkLTL(String ltl)
	{
		addLtl(ltl);
		generateDefines(functionMap);
		File promelaMachine  = new File(tempDir+File.separator+"promelaMachine");
		try {
			OutputUtil.write(defines.concat(sw.toString()), promelaMachine);
		} catch (IOException e) {
			Helper.throwUnchecked("could not run Spin", e);
		}
		return runSpin('s');
	}

	private void createInverseMap() 
	{
		inverseFunctionMap = new TreeMap<Integer, String>();
		for (String key : functionMap.keySet()) {
			inverseFunctionMap.put(functionMap.get(key), key);
		}
	}

	/*
	 * returns a set of counter examples
	 */
	private SpinResult runSpin(char safetyLiveness) 
	{
		SpinResult sr = new SpinResult();
		List<String[]> cmdArray = new ArrayList<String[]>();
		for(File f:new File(statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)).listFiles(new FileFilter(){
			public @Override boolean accept(File pathName) {
				return pathName.canRead() && pathName.isFile() &&
				pathName.getAbsolutePath().endsWith(".trail");
			}}))
			f.delete();

		int counter = 0;
		//cmdArray.add(0, (String[]) Arrays.asList("rm", "*.trail").toArray());
		cmdArray.add(counter++, (String[]) Arrays
				.asList("spin", "-Z", "promelaMachine").toArray());
		cmdArray.add(counter++, (String[]) Arrays.asList("spin", "-a", "-X",
				"promelaMachine").toArray());
		if( safetyLiveness == 's'){ //compile pan for checking safety properties
			cmdArray.add(counter++, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
				"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE",  "-DSAFETY", "-DNXT", "-DBFS", "-DREACH",
				//"-DNOREDUCE", 
				"-DNOFAIR", "pan.c").toArray());
			cmdArray.add(counter++, (String[]) Arrays.asList(tempDir+File.separator+"pan", "-v", "-X",
				"-m10000", "-w19", "-A", "-e", "-c0").toArray()); //"-i"
		}
		else{ //compile pan for checking liveness properties
			cmdArray.add(counter++, (String[]) Arrays.asList("gcc", "-w", "-o", "pan",
					"-D_POSIX_SOURCE", "-DMEMLIM=128", "-DXUSAFE", "-DNXT",
					//"-DNOREDUCE", 
					"-DNOFAIR", "pan.c").toArray());
			cmdArray.add(counter++, (String[]) Arrays.asList(tempDir+File.separator+"pan", "-v", "-X",
					"-m10000", "-w19", "-a", "-c1").toArray());
		}
		for (int i = 0; i < counter; i++) {
			try {
				String line;
				ProcessBuilder pb = new ProcessBuilder(cmdArray.get(i));
				pb.directory(new File(tempDir));
				Process proc = pb.start();
				InputStreamReader tempReader = new InputStreamReader(
	                new BufferedInputStream(proc.getInputStream()));
	            BufferedReader reader = new BufferedReader(tempReader); 
	            boolean foundNoError = false;
	            while ((line = reader.readLine()) != null) {
	            	if(line.contains("errors: 0")){
	            		foundNoError = true;
	            	}
	            	//System.out.println(line);
					
				}
	            
	            if (foundNoError)
	            {
            		sr.setCounters(new ArrayList<List<Label>>());
            		sr.setPass(true);
            		return sr;
	            }
	            	
			} catch (Throwable e) {
				e.printStackTrace();
			}
			
		}
		sr.setPass(false);
		sr.setCounters(getCounterExamples());
		return sr;
	}
	
	

	private List<List<Label>> getCounterExamples()
	{
		Set<List<Label>> counterExamples = new HashSet<List<Label>>();
		String[] filelist;
		File f = new File(tempDir);		
		filelist = f.list(new TrailFileFilter());
		for(int i=0; i< filelist.length; i++){
			List<Label> counterExample = getCounterExample(i,config);
			if(!counterExample.isEmpty())
				counterExamples.add(counterExample);
		}
		return sort(counterExamples);
	}
	
	public class TrailFileFilter implements FilenameFilter 
	{ 
	   public @Override boolean accept(@SuppressWarnings("unused") java.io.File f, java.lang.String g) 
	    { 
	       return (g.indexOf(".trail") != -1);
	   } 
	}

	
	private static int numAcceptingSuccessors(DirectedSparseVertex v){
		int succs = 0;
		@SuppressWarnings("unchecked")
		Iterator<DirectedEdge> outgoingIt = v.getOutEdges().iterator();
		while(outgoingIt.hasNext()){
			DirectedEdge e = outgoingIt.next();
			if(DeterministicDirectedSparseGraph.isAccept(e.getDest()))
				succs++;
		}
		return succs;
	}
	
	private void generatePromela(DirectedSparseGraph g) {
		Map<String, Integer> stateMap = new TreeMap<String, Integer>();
		functionMap = new HashMap<String, Integer>();
		setup(g, stateMap);
		@SuppressWarnings("unchecked")
		Iterator<DirectedSparseVertex> stateIt = g.getVertices().iterator();
		
		while (stateIt.hasNext()) {
			DirectedSparseVertex v = stateIt.next();
			
			String currentState = v.getUserDatum(JUConstants.LABEL).toString();
			if(v.containsUserDatumKey(JUConstants.INITIAL))
				currentState = "Init";
			if (!stateMap.keySet().contains(currentState)) {
				stateMap.put(currentState, Integer.valueOf(stateCounter));
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
				@SuppressWarnings("unchecked")
				Iterator<DirectedEdge> outEdges = v.getOutEdges().iterator();
				while(outEdges.hasNext()){
					DirectedEdge e = outEdges.next();
					@SuppressWarnings("unchecked")
					Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
					
					if(!DeterministicDirectedSparseGraph.isAccept(e.getDest()))
							continue;
					String toState = e.getDest().getUserDatum(JUConstants.LABEL).toString();
					if(e.getDest().containsUserDatumKey(JUConstants.INITIAL))
						toState = "Init";
					if (!stateMap.keySet().contains(toState)) {
						stateMap.put(toState, Integer.valueOf(stateCounter));
						stateCounter++;
					}
					Iterator<Label> labelIt = labels.iterator();
					while (labelIt.hasNext()) {
						Label label = labelIt.next();
						if (!functionMap.keySet().contains(label)) {
							functionMap.put(label.toErlangTerm(), Integer.valueOf(functionCounter));
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
	
	private void generatePromela(List<Label> question) {
		functionMap = new TreeMap<String, Integer>();
		sw.write("int input=50000;\nproctype machine(){\n");
		Iterator<Label> questionIt = question.iterator();
		
		while (questionIt.hasNext()) {
			Label symb = questionIt.next();
			if (!functionMap.keySet().contains(symb)) {
				functionMap.put(symb.toErlangTerm(), Integer.valueOf(functionCounter));
				functionCounter++;
			}
			sw.write("input="+functionMap.get(symb)+";\n");
			
		}
		
		sw.write("}\n\ninit {\nrun machine();\n}");
		printLegend(sw, functionMap);
		
	}

	private void addLtl(String ltl) {
		try {
			ensureApplicable(ltl);
			StringBuffer output = LowLevel.exec(ltl);
			sw.write("\n" + output.toString());
		} catch (Exception e) {e.printStackTrace();}
	}
	
	private void ensureApplicable(String ltl){
		String[] splitString = ltl.split("\\p{Punct}|X|U|V");
		for (String string : splitString) {
			if(string.trim().equals("")||string.trim().equals("[]"))
				continue;
			else if(!functionMap.keySet().contains(string)){
				functionMap.put(string, Integer.valueOf(functionCounter));
				functionCounter++;
			}
		}
	}

	private void generateDefines(Map<String, Integer> functionMapArg) {

		for (String key : functionMapArg.keySet()) {
			defines = defines.concat("#define " + key + "\t" + "(input == "
					+ functionMapArg.get(key) + ")\n");
		}
	}


	private static void printLegend(StringWriter swArg,
			Map<String, Integer> functionMapArg) {
		Iterator<String> keyIt = functionMapArg.keySet().iterator();
		swArg.write("\n/*");
		while (keyIt.hasNext()) {
			String key = keyIt.next();
			swArg.write("\n" + key + " - " + functionMapArg.get(key));
		}
		swArg.write("\n*/");
	}
	
	private List<Label> getCounterExample(int i,Configuration configArg)
	{
		List<Label> counterExample = new ArrayList<Label>();
		String[] trace = new String[]{"spin", "-t"+(i+1), "-p", "-c", "promelaMachine"};
		LinkedList<String> SpinData = new LinkedList<String>(); 
		try {
			String line;
			ProcessBuilder pb = new ProcessBuilder(trace);
			pb.directory(new File(tempDir));
			
			Process proc = pb.start();
			BufferedReader input = new BufferedReader(new InputStreamReader(
					proc.getInputStream()));
			while ((line = input.readLine()) != null) {
				SpinData.add(line);
				//System.out.println(line);
				if(line.contains("trail ends after"))
					break;
				if (line.contains("<valid end state>")&&line.contains("proc  1")){
					return new ArrayList<Label>(); //don't want to return this counterexample
				}
				else if (line.contains("[input")) {
					int inputIndex = line.indexOf("[input =") + 8;
					int closingBracket = line.indexOf("]", inputIndex);
					String function = inverseFunctionMap.get(Integer
							.valueOf(line.substring(inputIndex,
									closingBracket).trim()));
					if(function !=null)
						counterExample.add(AbstractLearnerGraph.generateNewLabel(function,configArg,converter));
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

	private void setup(DirectedSparseGraph g, Map<String, Integer> stateMap) {
		DirectedSparseVertex v = (DirectedSparseVertex) DeterministicDirectedSparseGraph
				.findInitial(g);
		String state = v.getUserDatum(JUConstants.LABEL).toString();
		if (!stateMap.keySet().contains(state)) {
			stateMap.put(state, Integer.valueOf(stateCounter));
			stateCounter++;
		}
		sw.write("int input=50000;\nproctype machine(){\ngoto Init;\n");
	}

}
