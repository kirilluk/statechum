/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

import edu.uci.ics.jung.graph.Graph;

import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.util.OutputUtil;

public class ForestFireIntermediateNegativesGenerator extends ForestFireLabelledStateMachineGenerator {

	public ForestFireIntermediateNegativesGenerator(double forwards,double backwards, double argSelfLoop, double argParallel, int alphabetSize, int seed) 
	{
		super(forwards, backwards,argSelfLoop,  argParallel,  alphabetSize, seed);
	}
	


	@Override
	protected void annotateVertex(DeterministicVertex vertex)
	{
		super.annotateVertex(vertex);
		vertex.setAccept(randomInt(2) > 0);
	}

	
	public static void main(String args[])
	{
		/*for(double f=0.6;f < 1;f+=0.05)
		{
			final int nr = 20;
			double accumMachine = 0, accumReduced = 0, statesMachine = 0, statesReduced = 0;
			for(int counter=0;counter < nr;++counter)
			{
				ForestFireIntermediateNegativesGenerator gen = new ForestFireIntermediateNegativesGenerator(f,1,0.2,10,0);
				LearnerGraph fsm = gen.buildMachine(130);
				accumMachine += ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine);statesMachine+=gen.machine.getVertices().size();
				accumReduced += ForestFireStateMachineGenerator.getEffectiveDiameter(fsm.pathroutines.getGraph());statesReduced+=fsm.getStateNumber();
				OutputUtil.generatePajekOutput(fsm.pathroutines.getGraph(),""+counter);
				OutputUtil.generateADLOutput(fsm, fsm.pathroutines.computeAlphabet().size()+"-"+counter+".adl");
			}
			System.out.println(f+" effective dim orig: "+accumMachine/nr+" effective dim reduced: "+accumReduced/nr+" statesOrig: "+statesMachine/nr+" statesReduced: "+statesReduced/nr);
		}*/
		int avgSize = 0;
		ArrayList<String> graphs = new ArrayList<String>();
		int counter = 0;
		int seed = 0;
		int alphabet = 50;
		while(counter<20){
			ForestFireIntermediateNegativesGenerator gen = new ForestFireIntermediateNegativesGenerator(0.31,0.385,0.2,0.2,alphabet,seed);
			LearnerGraph fsm = gen.buildMachine(160);
			if(fsm.getStateNumber()>40 && fsm.getStateNumber()<60 && fsm.pathroutines.computeAlphabet().size() == alphabet){
				String name = fsm.pathroutines.computeAlphabet().size()+"-"+counter;
				OutputUtil.generateADLOutput(fsm, name+".adl");
				OutputUtil.generatePajekOutput(fsm.pathroutines.getGraph(),name);
				avgSize+=fsm.getStateNumber();
				graphs.add(name);
				counter++;
				Visualiser.updateFrame(fsm, null);
				Visualiser.waitForKey();
			}
			seed++;
			//System.out.println(fsm.pathroutines.computeAlphabet().size() + ", "+fsm.getStateNumber());
			
		}
		printResults(graphs);
		System.out.println(avgSize/20);
		//Visualiser.updateFrame(fsm, null);
		//Visualiser.waitForKey();
	}
}
