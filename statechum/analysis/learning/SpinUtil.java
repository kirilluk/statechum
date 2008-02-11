package statechum.analysis.learning;
import java.io.*;
import java.util.*;

import statechum.JUConstants;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.*;

public class SpinUtil {
	
	public static void generatePromela(DirectedSparseGraph g){
		StringWriter sw = setup();
		Iterator<DirectedEdge> edgeIt = g.getEdges().iterator();
		HashMap<String,Integer> functionMap = new HashMap<String,Integer>();
		HashMap<String,Integer> stateMap = new HashMap<String,Integer>();
		int functionCounter =0, stateCounter = 0;
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
			}
			
		}
		sw.write("\n\t}\n\tfi\n\t}\n\tod\n}\n");
		sw = addInit(sw);
		//sw = printLegend(sw, functionMap);
		try{
			FileOutputStream fos = new FileOutputStream("promelaMachine");
			OutputStreamWriter out= new OutputStreamWriter(fos, "UTF-8");
			out.write(sw.toString());
			out.close();
			fos.close();
		}catch (Exception e){e.printStackTrace();}
	}
	
	private static StringWriter addInit(StringWriter sw){
		sw.write("\ninit {\nrun machine();\n}");
		return sw;
	}
	
	private static StringWriter printLegend(StringWriter sw, HashMap functionMap){
		Iterator<String> keyIt = functionMap.keySet().iterator();
		sw.write("\n/*");
		while(keyIt.hasNext()){
			String key = keyIt.next();
			sw.write("\n"+key+" - "+ functionMap.get(key));
		}
		sw.write("\n*/");
		return sw;
	}
	
	private static StringWriter setup(){
		StringWriter sw = new StringWriter();
		sw.write("proctype machine(){\n\tint state=0;\n\tdo{\n\tint input;\n\tif{");
		return sw;
	}

}
