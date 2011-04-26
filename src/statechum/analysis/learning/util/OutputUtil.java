package statechum.analysis.learning.util;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import statechum.JUConstants;
import statechum.Label;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;

public class OutputUtil {
	
	public static void generateTextOutput(DirectedSparseGraph g, String name){
		StringWriter graphout = traverseGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generateADLOutput(LearnerGraph g, String name){
		StringWriter graphout = new StringWriter();
		graphout.write(new LearnerGraphND(g,g.config).pathroutines.toADL());
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generateDotOutput(DirectedSparseGraph g, String name){
		StringWriter graphout = dotGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
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
				Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<Label> labelIt = labels.iterator();
        		while(labelIt.hasNext()){
        			graphout.write("\n"+from+" "+labelIt.next().toErlangTerm()+" "+to);
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
        		Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<Label> labelIt = labels.iterator();
        		graphout.write("\n"+from+" -> "+to+"[label=\"");
        		while(labelIt.hasNext()){
        			graphout.write(labelIt.next().toErlangTerm()+"\\n" );
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
			String labelAsString = null;	
			if(v.getUserDatum(JUConstants.INITIAL)!=null)
				labelAsString = "init";
			else
				labelAsString = v.toString();
			graphout.write("\n"+(vertexList.indexOf(v)+1) + " \""+labelAsString+"\"");
		}
		graphout.write("\n*Arcs");
		for (DirectedSparseEdge e : (Iterable<DirectedSparseEdge>)g.getEdges()) {
			Vertex dest = e.getDest();
			//if(!((Boolean)dest.getUserDatum(JUConstants.ACCEPTED)).booleanValue())
			//	continue;
			String from = String.valueOf((vertexList.indexOf(e.getSource())+1));
			String to = String.valueOf((vertexList.indexOf(e.getDest())+1));
			if(e.containsUserDatumKey(JUConstants.LABEL)){
        		Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
        		Iterator<Label> labelIt = labels.iterator();
        		while(labelIt.hasNext()){
        			graphout.write("\n"+from+" "+to+" l \"" + labelIt.next().toErlangTerm()+"\"");
        		}
        	}
		}
		return graphout;
	}

}
