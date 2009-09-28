/* Copyright (c) 2006, 2007, 2008, 2009 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
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

package statechum.analysis.learning.experiments;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import cern.jet.random.Distributions;
import cern.jet.random.engine.MersenneTwister;
import cern.jet.random.engine.RandomEngine;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.util.OutputUtil;
import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;
import edu.uci.ics.jung.graph.impl.DirectedSparseVertex;
import edu.uci.ics.jung.utils.UserData;

/*
 * Generates a random graph using a modified version of the 
 *  Forest Fire model by Leskovic et al. The modification lies in the fact that
 *  the direction of the edges are switched to make sure that every node can be
 *  reached from the initial node.
 *  
 *  Edge labels are not added (See ForestFireLabelledStateMachineGenerator)
 */

public class ForestFireStateMachineGenerator {
	
	private double forwards, backwards;
	protected DirectedSparseGraph machine;
	protected List<DirectedSparseVertex> vertices;
	protected Set<DirectedSparseVertex> visited;
	protected RandomEngine generator;
	protected Map<Object,DirectedSparseVertex> labelmap;
	
	
	public ForestFireStateMachineGenerator(double forwards, double backwards) throws Exception{
		this.forwards = forwards;
		this.backwards = backwards;
		if(!(forwards > 0 && forwards < 1) || !(backwards > 0 && backwards <= 1)){throw new Exception("invalid scopes for backwards or forwards");};
		visited = new HashSet<DirectedSparseVertex>();
		machine = new DirectedSparseGraph();
		vertices = new ArrayList<DirectedSparseVertex>();
		generator  = new MersenneTwister();
	}

	protected LearnerGraph buildMachine(int size) throws Exception {
		addInitialNode();
		for(int i=0;i<size;i++){
			DirectedSparseVertex v = (DirectedSparseVertex) machine.addVertex(new DirectedSparseVertex());
			visited.add(v);
			DirectedSparseVertex random = selectRandom();
			addEdge(random, v);
			visited.add(random);
			spread(v,random);
			vertices.add(v);
			visited.clear();
		}
		return new LearnerGraph(machine,Configuration.getDefaultConfiguration());
	}

	protected boolean addEdge(DirectedSparseVertex v, DirectedSparseVertex w) {
		DirectedSparseEdge e = new DirectedSparseEdge(v,w);
		machine.addEdge(e);
		return true;
	}
	
	protected void addInitialNode(){
		DirectedSparseVertex v = new DirectedSparseVertex();
		v.setUserDatum(JUConstants.INITIAL, true, UserData.SHARED);
		v.setUserDatum(JUConstants.LABEL, String.valueOf(0), UserData.SHARED);
		machine.addVertex(v);
		vertices.add(v);
	}
	
	protected void spread(DirectedSparseVertex v, DirectedSparseVertex ambassador){
		int x = Distributions.nextGeometric((forwards/(1-forwards)), generator);
		int y = Distributions.nextGeometric((backwards*forwards)/(1-(backwards*forwards)), generator);
		Set<DirectedSparseVertex> selectedVertices = selectLinks(x,y,ambassador);
		if(selectedVertices.isEmpty()){
			return;
		}
		for (DirectedSparseVertex w : selectedVertices) {
			addEdge(v,w);
			visited.add(w);
		}
		for (DirectedSparseVertex w : selectedVertices) {
			spread(v,w);
		}
		
	}
	
	


	private Set<DirectedSparseVertex> selectLinks(int x, int y,
			DirectedSparseVertex ambassador) {
		HashSet<DirectedSparseVertex> vertices = new HashSet<DirectedSparseVertex>();
		Iterator<DirectedSparseEdge> inIt = ambassador.getInEdges().iterator();
		int added = 0;
		while(added<x&&inIt.hasNext()){
			DirectedSparseEdge e = inIt.next();
			DirectedSparseVertex v = (DirectedSparseVertex) e.getSource();
			if(!visited.contains(v)){
				vertices.add(v);
				added++;
			}
		}
		added=0;
		Iterator<DirectedSparseEdge> outIt = ambassador.getOutEdges().iterator();
		while(added<y&&outIt.hasNext()){
			DirectedSparseEdge e = outIt.next();
			DirectedSparseVertex v = (DirectedSparseVertex) e.getDest();
			if(!visited.contains(v)){
				vertices.add(v);
				added++;
			}
		}
		return vertices;
	}

	protected DirectedSparseVertex selectRandom(){
		int size = vertices.size();
		if(size ==1)
			return (DirectedSparseVertex)vertices.get(0);
		else{
			Random r = new Random();
			int index = r.nextInt(vertices.size()-1);
			return (DirectedSparseVertex)vertices.get(index);
		}
	}
	//0.46,0.92,17,seed
	private static ArrayList<String> generateGraphs(int numberOfGraphs, double forward, double backward, int alphabet, int uppersize) throws Exception{
		ArrayList<String> graphs = new ArrayList<String>();
		int seed = 0;
		Random r = new Random(0);
		for(int i=0;i<numberOfGraphs;i++){
			ForestFireIntermediateNegativesGenerator fsmg = new ForestFireIntermediateNegativesGenerator(forward,backward,alphabet,seed, new Boolean(r.nextBoolean()));
			LearnerGraph g = fsmg.buildMachine(uppersize);
			//ForestFireStateMachineGenerator fsmg = new ForestFireLabelledStateMachineGenerator(forward,backward,alphabet,seed);
			//LearnerGraph g = fsmg.buildMachine(uppersize);
			if(g!=null){
				String name = String.valueOf(i+"."+i);
				//OutputUtil.generatePajekOutput(g.pathroutines.getGraph(),name);
				OutputUtil.generateADLOutput(g, g.pathroutines.computeAlphabet().size()+"-"+i+".adl");
				graphs.add(name);
			}
			else{
				seed++;
				i--;
			}
				
		}
		return graphs;
	}
	
	public static void main(String[] args) throws Exception{
		int numberOfGraphs = Integer.valueOf(args[0]);
		double forward = Double.valueOf(args[1]);
		double backward = Double.valueOf(args[2]);
		int alphabet = Integer.valueOf(args[3]);
		int uppersize = Integer.valueOf(args[4]);
		ArrayList<String> graphs = generateGraphs(numberOfGraphs, forward, backward, alphabet, uppersize);
		printResults(graphs);	
	}
	
	private static void printResults(ArrayList<String> graphs){
		for (int i=0;i<graphs.size();i++) {
			System.out.println("synth."+graphs.get(i)+".net <- read.graph(\""+ graphs.get(i)+".net\", format=\"pajek\")");
		}
		System.out.print("synth.states <- c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("vcount(synth."+graphs.get(i)+".net)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else System.out.print(")\n");
		}
		System.out.print("synth.transitions <- c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("ecount(synth."+graphs.get(i)+".net)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else System.out.print(")\n");
		}
		System.out.print("synth.depth <- c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("max(shortest.paths(simplify(synth."+graphs.get(i)+".net,remove.loops=TRUE),v=V(synth."+graphs.get(i)+".net)[id==\"init\"], mode=\"out\"))");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else System.out.print(")\n");
		}
		System.out.print("synth.diameter <- c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("diameter(synth."+graphs.get(i)+".net)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else System.out.print(")\n");
		}
		System.out.print("synth.transitivity <- c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("transitivity(synth."+graphs.get(i)+".net)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else System.out.print(")\n");
		}
		System.out.print("stats.inout <- cbind(c(");
		for (int i=0;i<graphs.size();i++) {
			System.out.print("degree(synth."+graphs.get(i)+".net,mode=\"out\",loops=TRUE)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else{
				System.out.print("),c(");
				for (int j=0;j<graphs.size();j++) {
					System.out.print("degree(synth."+graphs.get(j)+".net,mode=\"in\",loops=TRUE)");
					if(j<graphs.size()-1)
						System.out.print(",");
					else{
						System.out.print("))\n");
					}
				}
			}
		}
	}

}
