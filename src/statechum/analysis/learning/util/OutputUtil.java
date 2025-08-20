package statechum.analysis.learning.util;

import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.graph.impl.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

import statechum.DeterministicDirectedSparseGraph;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.JUConstants;
import statechum.Label;
import statechum.LabelInputOutput;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.CachedData;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.collections.MapWithSearch;

public class OutputUtil {
	
	public static void generateTextOutput(DirectedSparseGraph g, String name) throws IOException{
		StringWriter graphout = traverseGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
		graphout.close();
	}
	
	public static void generateADLOutput(LearnerGraph g, String name) throws IOException{
		StringWriter graphout = new StringWriter();
		graphout.write(new LearnerGraphND(g,g.config).pathroutines.toADL());
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
	}
	
	public static void generateDotOutput(DirectedSparseGraph g, String name) throws IOException{
		StringWriter graphout = dotGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
		graphout.close();
	}
	
	public static void generatePajekOutput(DirectedSparseGraph g, String name) throws IOException{
		StringWriter graphout = pajekGraph(g);
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name+".net";
		File outputMachine  = new File(fileRef);
		write(graphout.toString(), outputMachine);
		graphout.close();
	}
	
	public static void write(String string, File f) throws IOException{
		if(f.getParentFile()!=null)
			f.getParentFile().mkdirs();
		f.createNewFile();
		FileOutputStream fos = new FileOutputStream(f);
		OutputStreamWriter out = new OutputStreamWriter(fos, StandardCharsets.UTF_8);
		out.write(string);
		out.close();
		fos.close();
	}
	
	@SuppressWarnings("unchecked")
	protected static StringWriter traverseGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		for (DirectedSparseEdge e : (Iterable<DirectedSparseEdge>)g.getEdges()) {
			Vertex dest = e.getDest();
			if(!(Boolean) dest.getUserDatum(JUConstants.ACCEPTED))
				continue;
			String from = e.getSource().toString();
			String to = e.getDest().toString();
			if(e.containsUserDatumKey(JUConstants.LABEL)){
				Set<Label> labels = (Set<Label>)e.getUserDatum(JUConstants.LABEL);
                for (Label label : labels) {
                    graphout.write("\n" + from + " " + label.toErlangTerm() + " " + to);
                }
        	}
		}
		return graphout;
	}
	
	@SuppressWarnings("unchecked")
	protected static StringWriter dotGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		graphout.write("digraph dotMachine{");
		ArrayList<DeterministicVertex> vertexList = new ArrayList<>();
		for(DeterministicVertex v: (Iterable<DeterministicVertex>)g.getVertices()){
				vertexList.add(v);
		}
		for(Vertex v: (Iterable<DeterministicVertex>)g.getVertices()){
			Boolean accepted = (Boolean)v.getUserDatum(JUConstants.ACCEPTED);
			if(!v.toString().equals("Init") && accepted)
				graphout.write("\n"+vertexList.indexOf(v)+"[label=\"\" shape=\"circle\"]");
		}
		
		for (DeterministicEdge e : (Iterable<DeterministicEdge>)g.getEdges()) {
			DeterministicVertex dest = (DeterministicVertex) e.getDest();
			if(!(Boolean) dest.getUserDatum(JUConstants.ACCEPTED))
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

	/** Turns a graph into a .dot file, accounting for both i/o and LTS and supporting non-deterministic graphs.
	 *
	 * @param graph graph to convert
	 * @param collate_source_target_pairs whether to collate multiple transitions into a single label, where elements are separated with newlines.
	 * @return converted graph
	 */
	public static <TARGET_TYPE,CACHE_TYPE extends CachedData<TARGET_TYPE,CACHE_TYPE>> StringWriter dotGraphMealy(AbstractLearnerGraph<TARGET_TYPE,CACHE_TYPE> graph, boolean collate_source_target_pairs) {
		StringWriter graphout = new StringWriter();
		String name = graph.getName() == null?"graph":graph.getName();
		graphout.write("digraph ");graphout.write(name);graphout.write(" {\n");
		graphout.write("\trankdir=LR;\n" +
				"\tnode [shape=circle, style=filled, fillcolor=white];\n");
		if (!graph.transitionMatrix.isEmpty()) {
			if (graph.getInit() == null)
				throw new IllegalArgumentException("Graph has no init");
			writeOutDefinitionOfVertex(graph.getInit(), graphout);
		}

		for(DeterministicDirectedSparseGraph.CmpVertex vert:graph.transitionMatrix.keySet())
			if (vert != graph.getInit())
				writeOutDefinitionOfVertex(vert, graphout);

		for(Map.Entry<DeterministicDirectedSparseGraph.CmpVertex, MapWithSearch<Label, Label, TARGET_TYPE>> entry:graph.transitionMatrix.entrySet())
		if (collate_source_target_pairs) {
			Map<DeterministicDirectedSparseGraph.CmpVertex,StringWriter> targetToLabel = new TreeMap<>();
			for (Map.Entry<Label, TARGET_TYPE> transition : entry.getValue().entrySet())
				for (DeterministicDirectedSparseGraph.CmpVertex v : graph.getTargets(transition.getValue())) {
					StringWriter writer = targetToLabel.get(v);
					if (writer == null) {
						writer = new StringWriter();TransitionLabelToText(transition.getKey(),writer);
						targetToLabel.put(v, writer);
					}
					else {
						writer.write("\\n");
						TransitionLabelToText(transition.getKey(), writer);
					}
				}

			for(Map.Entry<DeterministicDirectedSparseGraph.CmpVertex,StringWriter> targetToTransitions:targetToLabel.entrySet()) {
				graphout.write("\t\"");
				graphout.write(entry.getKey().getStringId());
				graphout.write('\"');
				graphout.write("->");
				graphout.write('\"');
				graphout.write(targetToTransitions.getKey().getID().getStringId());
				graphout.write('\"');
				graphout.write(" [label=\"");
				graphout.write(targetToTransitions.getValue().toString());
				graphout.write("\"];\n");
			}
		}
		else
		{
			for (Map.Entry<Label, TARGET_TYPE> transition : entry.getValue().entrySet())
				for (DeterministicDirectedSparseGraph.CmpVertex v : graph.getTargets(transition.getValue())) {
					graphout.write("\t\"");
					graphout.write(entry.getKey().getStringId());
					graphout.write('\"');
					graphout.write("->");
					graphout.write('\"');
					graphout.write(v.getID().getStringId());
					graphout.write('\"');
					graphout.write(" [label=\"");
					TransitionLabelToText(transition.getKey(), graphout);
					graphout.write("\"];\n");
				}
		}
		graphout.write("}\n");
		return graphout;
	}

	private static void TransitionLabelToText(Label label, StringWriter graphout) {
		if (label instanceof LabelInputOutput) {
			LabelInputOutput io = (LabelInputOutput) label;
			graphout.write(io.input);
			graphout.write('/');
			graphout.write(io.output);
		} else
			graphout.write(label.toString());
	}

	private static void writeOutDefinitionOfVertex(DeterministicDirectedSparseGraph.CmpVertex vert, StringWriter graphout) {
		graphout.write("\t\"");
		graphout.write(vert.getID().getStringId());
		graphout.write('\"');
		graphout.write(" [shape=");
		graphout.write(vert.isAccept()?"doublecircle":"square");
		if (vert.getColour() != null)
			switch(vert.getColour()) {
				case RED:
					graphout.write(",fillcolor=red");break;
				case BLUE:
					graphout.write(",fillcolor=blue");break;
				default:
					break;
			}
		graphout.write("];\n");
	}

	@SuppressWarnings("unchecked")
	protected static StringWriter pajekGraph(DirectedSparseGraph g){
		StringWriter graphout = new StringWriter(); 
		graphout.write("*Network\n*Vertices "+g.numVertices());
		ArrayList<DeterministicVertex> vertexList = new ArrayList<>();
		for(DeterministicVertex v: (Iterable<DeterministicVertex>)g.getVertices()){
				vertexList.add(v);
		}
		for(DeterministicVertex v: (Iterable<DeterministicVertex>)g.getVertices()){
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
		for (DeterministicEdge e : (Iterable<DeterministicEdge>)g.getEdges()) {
			//Vertex dest = e.getDest();
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
