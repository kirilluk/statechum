package statechum.analysis.learning.util;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import java.io.*;
import java.util.HashSet;
import java.util.Iterator;

import statechum.JUConstants;

public class OutputUtil {
	
	public static void generateTextOutput(DirectedSparseGraph g){
		StringWriter graphout = traverseGraph(g);
		String fileRef = "temp"+System.getProperty("file.separator")+"textMachineOutput";
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void write(String string, File f){
		try {
			if(f.getParentFile()!=null)
				f.getParentFile().mkdirs();
			f.createNewFile();
			FileOutputStream fos = new FileOutputStream(f);
			OutputStreamWriter out = new OutputStreamWriter(fos, "UTF-8");
			out.write(string);
			out.close();
			fos.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	protected static StringWriter traverseGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		for (DirectedSparseEdge e : (Iterable<DirectedSparseEdge>)g.getEdges()) {
			Vertex dest = e.getDest();
			if(!((Boolean)dest.getUserDatum(JUConstants.ACCEPTED)).booleanValue())
				continue;
			String from = e.getSource().toString();
			String to = e.getDest().toString();
			if(e.containsUserDatumKey(JUConstants.LABEL)){
        		HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<String> labelIt = labels.iterator();
        		while(labelIt.hasNext()){
        			graphout.write("\n"+from+" "+labelIt.next()+" "+to);
        		}
        	}
		}
		return graphout;
	}

}
