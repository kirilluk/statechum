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

package statechum.analysis.learning.rpnicore;

import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import edu.uci.ics.jung.utils.*;
import edu.uci.ics.jung.statistics.*;
import edu.uci.ics.jung.algorithms.shortestpath.DijkstraDistance;

import java.util.*;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.xmachine.model.testset.*;

public class RandomPathGenerator {
	
	protected DirectedSparseGraph g;
	private PTASequenceSet sPlus, sMinus;
	private int extradiameter=0;
	
	/** The random number generator passed in is used to generate walks; one can pass a mock in order to 
	 * produce walks devised by a tester. Note that the object will be modified in the course of walks thanks
	 * to java's Random being non-serialisable.
	 *  
	 * @param baseGraph
	 * @param randomGenerator
	 * @param extraToDiameter the length of paths will be diameter plus this value.
	 */ 
	public RandomPathGenerator(DirectedSparseGraph baseGraph, Random randomGenerator, int number,  int extraToDiameter) {
		pathRandomNumberGenerator = randomGenerator;
		sPlus = new PTASequenceSet();
		sMinus = new PTASequenceSet();
		g = baseGraph;
		this.extradiameter = extraToDiameter;
		this.populateRandomWalksC(number, diameter(g)+extradiameter);
		this.populateNegativeRandomWalksC(number, diameter(g)+extradiameter);
	}
	
	public static int diameter(DirectedSparseGraph graph)
	{// TODO: to rewrite using a flowgraph or not?
		DijkstraDistance dd = new DijkstraDistance(graph);
		Collection<Double> distances = dd.getDistanceMap(DeterministicDirectedSparseGraph.findInitial(graph)).values();
		ArrayList<Double> distancesList = new ArrayList<Double>(distances);
		Collections.sort(distancesList);
		return distancesList.get(distancesList.size()-1).intValue();
	}

	private void populateRandomWalksC(int number, int maxLength){
		int counter=0, unsucc = 0;
		LearnerGraph fsm = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		Random length = new Random(0);
		while(counter<number){
			List<String> path = new ArrayList<String>(maxLength);
			CmpVertex current = fsm.init;
			if(unsucc>100)
				return;
			int randomLength =  0;
			while(randomLength == 0)
				randomLength=length.nextInt(maxLength+1);
			for(int i=0;i<randomLength;i++){
				Map<String,CmpVertex> row = fsm.transitionMatrix.get(current);
				if(row.isEmpty())
					break;
				String nextInput= (String)pickRandom(row.keySet());
				path.add(nextInput);
				current = row.get(nextInput);
			}
			int oldSize = sPlus.size();	
			sPlus.add(new ArrayList<String>(path));
			if(sPlus.size()>oldSize){
				counter++;
				unsucc=0;
			}
			else{
				unsucc++;
				
			}
		}
	}

	private void populateNegativeRandomWalksC(int number, int maxLength){
		int counter=0, unsucc = 0;
		LearnerGraph fsm = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		Set<String> alphabet = fsm.wmethod.computeAlphabet();
		Random length = new Random(0);
		while(counter<number){
			boolean skip = false;
			List<String> path = new ArrayList<String>(maxLength);
			CmpVertex current = fsm.init;
			if(unsucc>100)
				return;
			int randomLength =  0;
			while(randomLength == 0)
				randomLength=length.nextInt(maxLength+1)+1;
			for(int i=0;i<randomLength;i++){
				Map<String,CmpVertex> row = fsm.transitionMatrix.get(current);
				if(row.isEmpty()){
					skip = true;
					break;
				}
				if(i==randomLength-1){
					Set<String> negatives = new HashSet<String>();
					negatives.addAll(alphabet);
					negatives.removeAll(row.keySet());
					if(negatives.isEmpty()){
						skip = true;
						break;
					}
					path.add((String)pickRandom(negatives));
				}
				else{
					String nextInput= (String)pickRandom(row.keySet());
					path.add(nextInput);
					current = row.get(nextInput);
				}
			}
			if(skip){
				skip = false;
				continue;
			}
			int oldSize = sMinus.size();	
			sMinus.add(new ArrayList<String>(path));
			if(sMinus.size()>oldSize){
				counter++;
				unsucc=0;
			}
			else{
				unsucc++;
				
			}
		}
	}
	
	public Collection<List<String>> makeCollectionNegative(Collection<List<String>> pathCollection, int negativesPerPositive){
		Set<List<String>> negativePaths = new HashSet<List<String>>(); 
		Iterator<List<String>> collectionIt = pathCollection.iterator();
		LearnerGraph fsm = new LearnerGraph(g,Configuration.getDefaultConfiguration());
		while(collectionIt.hasNext()){
			List<String> path = collectionIt.next();
			CmpVertex current = fsm.init;
			Map<String,CmpVertex> row = null;
			for(int i=0;i<path.size();i++){
				row = fsm.transitionMatrix.get(current);
				String next = path.get(i);
				current = row.get(next);
			}
			for(int i=0;i<negativesPerPositive;i++){
				List<String> negativeString = makeNegative(fsm, path, current, negativePaths);
				if(negativeString.size()>0)
					negativePaths.add(negativeString);
			}
		}
		return negativePaths;
	}
	
	private List<String> makeNegative(LearnerGraph fsm, List<String> positivePath, CmpVertex current, Set<List<String>> negativePaths){
		Map<String,CmpVertex>row = fsm.transitionMatrix.get(current);
		List<String> negativePath = new ArrayList<String>();
		Set<String> alphabet = fsm.wmethod.computeAlphabet();
		Vector<String> negatives = new Vector<String>();
		negatives.addAll(alphabet);
		negatives.removeAll(row.keySet());
		negativePath.addAll(positivePath);
		for(int i=0;i<negatives.size();i++){
			String negative = negatives.get(i);
			negativePath.add(negative);
			if(!negativePaths.contains(negativePath))
				return negativePath;
			else
				negativePath.remove(negativePath.size()-1);
		}
		return new ArrayList<String>();
	}

	
	private final Random pathRandomNumberGenerator; 
	
	private Object pickRandom(Collection c){
		Object[] array = c.toArray();
		if(array.length==1)
			return array[0];
		else{
			int random = pathRandomNumberGenerator.nextInt(array.length);
			return array[random];
		}
	}

	public Collection<List<String>> getSPlus() {
		return sPlus;
	}
	
	public Collection<List<String>> getNegativePaths(){
		Collection<List<String>> allPaths = new LinkedList<List<String>>();
		
		allPaths.addAll(sMinus.getData());
		return allPaths;
	}
	
	public Collection<List<String>> getAllPaths(){
		Collection<List<String>> allPaths = new LinkedList<List<String>>();
		
		allPaths.addAll(sPlus.getData());
		return allPaths;
	}
	
	

}
