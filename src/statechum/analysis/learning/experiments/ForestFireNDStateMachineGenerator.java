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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import cern.jet.random.Distributions;
import cern.jet.random.engine.MersenneTwister;
import cern.jet.random.engine.RandomEngine;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.Label;
import statechum.StringVertex;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

/*
 * Generates a random graph using a modified version of the 
 *  Forest Fire model by Leskovic et al. The modification lies in the fact that
 *  the direction of the edges are switched to make sure that every node can be
 *  reached from the initial node.
 *  
 *  Edge labels are not added (See ForestFireLabelledStateMachineGenerator)
 */

public class ForestFireNDStateMachineGenerator {
	
	private double forwards, backwards, selfLoop;
	protected LearnerGraphND machine;
	protected List<CmpVertex> vertices;
	protected Set<CmpVertex> visited;
	protected RandomEngine generator;
	protected Random boolGenerator;
	protected int alphabet;
	
	public ForestFireNDStateMachineGenerator(double argForward, double argBackward, double argSelfloop, int seed, int alphabet)
	{
		this.forwards = argForward;this.backwards = argBackward;selfLoop=argSelfloop;
		if(!(argForward > 0 && argForward < 1) || !(argBackward > 0 && argBackward <= 1))
			throw new IllegalArgumentException("invalid scopes for backwards or forwards");
		visited = new HashSet<CmpVertex>();
		machine = new LearnerGraphND(Configuration.getDefaultConfiguration());
		vertices = new ArrayList<CmpVertex>();
		generator  = new MersenneTwister(seed);
		this.alphabet = alphabet;
		boolGenerator = new Random(seed);
		CmpVertex v= AbstractLearnerGraph.generateNewCmpVertex(new VertexID(VertexID.VertKind.NEUTRAL,0), Configuration.getDefaultConfiguration());  //new ();
		annotateVertex(v);
		vertices.add(v);// permits v to be chosen as a target, creating self-loops
		machine.getTransitionMatrix().put(v,machine.createNewRow());
		machine.setInit(v);
	}

	/** Annotates a vertex with various things such as accept conditions. Expected to be overridden by subclasses. */
	protected void annotateVertex(@SuppressWarnings("unused") CmpVertex v)
	{
	}
	
	protected Map<VertexID,CmpVertex> labelmap;

	/** Adds the supplied number of states to the machine, connecting them to the surrounding ones via forest-fire.
	 * 
	 * @param size the number of states to add
	 * @return constructed machine.
	 * @throws Exception if something goes wrong.
	 */
	protected void buildGraph(int size) 
	{
		synchronized(AbstractLearnerGraph.syncObj)
		{// This kills multi-core operation but then with Jung there is no other choice - it simply does not
		 // support multi-core (internal vertex ID generation of Jung is not synchronized).
			
			labelmap = new HashMap<VertexID,CmpVertex>();
			for(int i=0;i<size-1;i++)
			{
				CmpVertex v=new StringVertex(new VertexID(VertexID.VertKind.NEUTRAL,i+1));
				annotateVertex(v);
				machine.getTransitionMatrix().put(v, machine.createNewRow());
				vertices.add(v);// permits v to be chosen as a target, creating self-loops
				this.labelmap.put(v.getID(), v);
				CmpVertex random = selectRandomVertex();

				machine.addTransition(machine.getTransitionMatrix().get(random), AbstractLearnerGraph.generateNewLabel(randomInt(alphabet-1),machine.config), v);
				//Visualiser.updateFrame(machine, null);
				if (Distributions.nextGeometric(1-selfLoop,generator)>0)
					machine.addTransition(machine.getTransitionMatrix().get(v), AbstractLearnerGraph.generateNewLabel(randomInt(alphabet-1),machine.config), v); 

				// if the above fails, we bail out via an IllegalArgumentException from selectRandom(), hence
				// at this point it is appropriate to assume that we were successful.
				visited.add(random);visited.add(v);
				spread(v,random);
				visited.clear();
			}
			
		}
	}
	
	private StringVertex selectRandomVertex() {
			StringVertex[] states = new StringVertex[vertices.size()];
			vertices.toArray(states);
			return states[randomInt(vertices.size()-1)];
	}

	protected LearnerGraphND buildMachine(int size) 
	{
		buildGraph(size);
		Configuration conf = Configuration.getDefaultConfiguration();
		//conf.setAllowedToCloneNonCmpVertex(true);
		return new LearnerGraphND(machine,conf);
	}
	
	
	
	
	protected void spread(CmpVertex v, CmpVertex ambassador)
	{
		int x = Distributions.nextGeometric((1-forwards),generator);
		
		int y = Distributions.nextGeometric(1-backwards*forwards,generator);
		//System.out.println(x+","+y);
		List<CmpVertex> selectedVertices = selectLinks(x,y,ambassador);
		if(selectedVertices.isEmpty())
			return;

		for (CmpVertex w : selectedVertices) {
			if(boolGenerator.nextBoolean())
				machine.addTransition(machine.getTransitionMatrix().get(v), AbstractLearnerGraph.generateNewLabel(randomInt(alphabet-1),machine.config), w);
			else
				machine.addTransition(machine.getTransitionMatrix().get(w), AbstractLearnerGraph.generateNewLabel(randomInt(alphabet-1),machine.config), v);
			visited.add(w);
		}
		
		for (CmpVertex w : selectedVertices) {
			spread(v,w);
		}
		
	}
	
	protected static int getEffectiveDiameter(DirectedSparseGraph machine)
	{
		DijkstraDistance p = new DijkstraDistance(machine);
		List<Integer> distances = new LinkedList<Integer>();
		for(DeterministicVertex v:(Set<DeterministicVertex>)machine.getVertices())
			for(DeterministicVertex vOther:(Set<DeterministicVertex>)machine.getVertices())
			{
				int length = 0;
				Number distance = p.getDistance(v, vOther);if (distance != null) length = distance.intValue();
				if (length > 0) // non-empty path
					distances.add(length);
				if (v == vOther)
					break;// we only process a triangular subset.
			}
		
		int result = 0;
		if (distances.size() == 1)
			result = distances.get(0);
		else
		if (distances.size() > 0)
		{
			Integer distancesArray [] = new Integer[distances.size()];distances.toArray(distancesArray);
			Arrays.sort(distancesArray);
			int position90 = (int)(distances.size()*0.9)-1;
	
			result = distancesArray[position90];
		}
		return result;
	}

	/** Given a collection of vertices and a number, this method randomly chooses the set number of vertices 
	 * from the collection.  
	 */
	protected Collection<CmpVertex> selectVertices(Collection<CmpVertex> argVertices, int x)
	{
		Set<Integer> chosenInts = new TreeSet<Integer>();int size = argVertices.size();
		if (size <= x) return argVertices;
		//if (size < x) throw new IllegalArgumentException("cannot return more vertices than there is in the set");
		List<CmpVertex> result = new LinkedList<CmpVertex>(); 
		final int attemptCounter = 10000;
		int i=0;
		for(i=0;i<attemptCounter && chosenInts.size() < x;++i)
			chosenInts.add(randomInt(size));
		if (i == attemptCounter) 
			throw new IllegalArgumentException("random number generator failure");
		CmpVertex verticesGiven[]=new CmpVertex[size];argVertices.toArray(verticesGiven);
		for(Integer chosen:chosenInts)
			result.add(verticesGiven[chosen]);
		return result;
	}
	
	/** Aims to help connecting the given ambassador node to the supplied number of 
	 * vertices in both directions, by choosing vertices to connect to. Only vertices  
	 * which have not been previously considered (that is, not in the visited set) can be chosen.
	 *  
	 * @param x the number of vertices to connect to, in the forward direction.
	 * @param y the number of vertices to connect to, in reverse.
	 * @param ambassador what to connect
	 * @return collection of vertices 
	 */
	private List<CmpVertex> selectLinks(int x, int y, CmpVertex ambassador) 
	{
		Map<CmpVertex, Map<Label,List<CmpVertex>>> preds = computePreds();
		// This one needs to choose vertices at random, not just choose first x/y vertices.
		List<CmpVertex> result = new ArrayList<CmpVertex>();
		Map<Label,List<CmpVertex>> incoming = preds.get(ambassador);
		if(incoming != null){
			result.addAll(selectVerticesFromSet(x, incoming));
		}
		Map<Label,List<CmpVertex>> outgoing = machine.getTransitionMatrix().get(ambassador);
		result.addAll(selectVerticesFromSet(y, outgoing));
		return result;
	}

	private List<CmpVertex> selectVerticesFromSet(int x, Map<Label, List<CmpVertex>> set) {
		List<CmpVertex> result = new ArrayList<CmpVertex>();
		List<CmpVertex> verticesToChooseFrom = new LinkedList<CmpVertex>();
		Iterator<Label> inIt = set.keySet().iterator();
		while(inIt.hasNext()){
			Label e = inIt.next();
			List<CmpVertex> verticesToBeAdded = new ArrayList<CmpVertex>();
			verticesToBeAdded.addAll(set.get(e));
			verticesToBeAdded.removeAll(visited);
			verticesToChooseFrom.addAll(verticesToBeAdded);
		}
		if (!verticesToChooseFrom.isEmpty()) 
			result.addAll(selectVertices(verticesToChooseFrom, x));
		return result;
	}

	private Map<CmpVertex, Map<Label,List<CmpVertex>>> computePreds() {
		Map<CmpVertex, Map<Label,List<CmpVertex>>> preds = new HashMap<CmpVertex,Map<Label,List<CmpVertex>>>();
		Map<CmpVertex, Map<Label,List<CmpVertex>>> matrix = machine.getTransitionMatrix();
		Set<CmpVertex> keys = matrix.keySet();
		for (CmpVertex cmpVertex : keys) {
			Map<Label,List<CmpVertex>> dests = matrix.get(cmpVertex);
			Set<Label> labels = dests.keySet();
			for (Label string : labels) {
				List<CmpVertex> destinations = dests.get(string);
				for (CmpVertex destination : destinations) {
					Map<Label,List<CmpVertex>> pred;
					if(preds.get(destination)==null){
						pred = new HashMap<Label,List<CmpVertex>>();
						pred.put(string, newListWithVertex(cmpVertex));
						
					}
					else{
						pred = preds.get(destination);
						if(pred.get(string) == null){
							pred.put(string, newListWithVertex(cmpVertex));
						}
						else{
							pred.get(string).add(cmpVertex);
						}
					}
					preds.put(destination, pred);
				}
			}
		}
		return preds;
	}

	private List<CmpVertex> newListWithVertex(CmpVertex cmpVertex) {
		List<CmpVertex> verticesToReturn = new ArrayList<CmpVertex>();
		verticesToReturn.add(cmpVertex);
		return verticesToReturn;
	}

	final long integerRange = -(long)Integer.MIN_VALUE+Integer.MAX_VALUE;
	
   /** Generates a random number in the range of 0..upTo-1
    * <p>
    * Description:<br>
    * Let the random number r be between 0 and 1 (inclusive), then 
	* r*size will be distributed between 0 and size, thus one way is to
	* do position = (int)(r*upTo), truncating floating-point numbers. There is a possibility that
	* the max value will be reached (albeit this will be rare), so we could do something like 
	* if (position == size) --position;
	* It is worth pointing out that if size==1, the above will still work.
	* Note that it does not make sense to use position = (int)(r*upTo-1) because the value size-1
	* will be rarely reached.
	* If we wish to avoid floating-point numbers, an option is to do 
	* r=((long)generator.nextInt()-(long)Integer.MIN_VALUE)/integerRange
	*/
	protected int randomInt(int upTo)
	{
		if (upTo <= 0) throw new IllegalArgumentException("upTo has to be above zero");
		long result = ((long)generator.nextInt()-(long)Integer.MIN_VALUE)*upTo/integerRange;
		assert result >=0 && result <= upTo;
		if (result == upTo) --result;
		return (int)result;
	}
		/*
	//0.46,0.92,17,seed
	private static ArrayList<String> generateGraphs(int numberOfGraphs, double forward, double backward, int alphabet, int uppersize,Configuration config) throws Exception{
		ArrayList<String> graphs = new ArrayList<String>();
		int seed = 0;
		for(int i=0;i<numberOfGraphs;i++){
			ForestFireIntermediateNegativesGenerator fsmg = new ForestFireIntermediateNegativesGenerator(forward,backward,0.35,0.2,alphabet,seed,config);
			LearnerGraph g = fsmg.buildMachine(uppersize);
			//ForestFireStateMachineGenerator fsmg = new ForestFireLabelledStateMachineGenerator(forward,backward,alphabet,seed);
			//LearnerGraph g = fsmg.buildMachine(uppersize);
			if(g!=null){
				String name = String.valueOf(i+"."+i);
				OutputUtil.generatePajekOutput(g.pathroutines.getGraph(),name);
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
	*/
	protected static void printResults(ArrayList<String> graphs){
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
			System.out.print("degree(synth."+graphs.get(i)+".net,mode=\"in\",loops=TRUE)");
			if(i!=graphs.size()-1)
				System.out.print(",");
			else{
				System.out.print("),c(");
				for (int j=0;j<graphs.size();j++) {
					System.out.print("degree(synth."+graphs.get(j)+".net,mode=\"out\",loops=TRUE)");
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
