package statechum.analysis.learning.util;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import statechum.JUConstants;

public class OutputUtil {
	
	public static void generateTextOutput(DirectedSparseGraph g){
		StringWriter graphout = traverseGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"textMachineOutput";
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generateDotOutput(DirectedSparseGraph g){
		StringWriter graphout = dotGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"dotOutput.dot";
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generatePajekOutput(DirectedSparseGraph g){
		StringWriter graphout = pajekGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+"pajekOutput.dot";
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generatePajekOutput(DirectedSparseGraph g, String name){
		StringWriter graphout = pajekGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name+".net";
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
	
	protected static StringWriter dotGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		graphout.write("digraph dotMachine{");
		ArrayList<DirectedSparseVertex> vertexList = new ArrayList<DirectedSparseVertex>();
		for(DirectedSparseVertex v: (Iterable<DirectedSparseVertex>)g.getVertices()){
				vertexList.add(v);
		}
		for(Vertex v: (Iterable<DirectedSparseVertex>)g.getVertices()){
			Boolean accepted = (Boolean)v.getUserDatum(JUConstants.ACCEPTED);
			if(!v.toString().equals("Init") && accepted.booleanValue())
				graphout.write("\n"+vertexList.indexOf(v)+"[label=\"\" shape=\"circle\"]");
		}
		
		for (DirectedSparseEdge e : (Iterable<DirectedSparseEdge>)g.getEdges()) {
			Vertex dest = e.getDest();
			if(!((Boolean)dest.getUserDatum(JUConstants.ACCEPTED)).booleanValue())
				continue;
			String from = String.valueOf(vertexList.indexOf(e.getSource()));
			String to = String.valueOf(vertexList.indexOf(e.getDest()));
			if(e.containsUserDatumKey(JUConstants.LABEL)){
        		HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<String> labelIt = labels.iterator();
        		graphout.write("\n"+from+" -> "+to+"[label=\"");
        		while(labelIt.hasNext()){
        			graphout.write(labelIt.next()+"\\n" );
        		}
        		graphout.write("\"]");
        	}
		}
		graphout.write("\n}");
		return graphout;
	}
	
	protected static StringWriter pajekGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		graphout.write("*Network\n*Vertices "+g.numVertices());
		ArrayList<DirectedSparseVertex> vertexList = new ArrayList<DirectedSparseVertex>();
		for(DirectedSparseVertex v: (Iterable<DirectedSparseVertex>)g.getVertices()){
				vertexList.add(v);
		}
		for(Vertex v: (Iterable<DirectedSparseVertex>)g.getVertices()){
			//Boolean accepted = (Boolean)v.getUserDatum(JUConstants.ACCEPTED);
			//if(!v.toString().equals("Init") && accepted.booleanValue())
			String label = null;	
			if(v.getUserDatum(JUConstants.INITIAL)!=null)
				label = "init";
			else
				label = v.toString();
			graphout.write("\n"+(vertexList.indexOf(v)+1) + " \""+label+"\"");
		}
		graphout.write("\n*Arcs");
		for (DirectedSparseEdge e : (Iterable<DirectedSparseEdge>)g.getEdges()) {
			Vertex dest = e.getDest();
			//if(!((Boolean)dest.getUserDatum(JUConstants.ACCEPTED)).booleanValue())
			//	continue;
			String from = String.valueOf((vertexList.indexOf(e.getSource())+1));
			String to = String.valueOf((vertexList.indexOf(e.getDest())+1));
			if(e.containsUserDatumKey(JUConstants.LABEL)){
        		HashSet<String> labels = (HashSet<String>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<String> labelIt = labels.iterator();
        		while(labelIt.hasNext()){
        			graphout.write("\n"+from+" "+to+" l \"" + labelIt.next()+"\"");
        		}
        	}
		}
		return graphout;
	}

}
