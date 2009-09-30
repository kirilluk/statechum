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

import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class ForestFireIntermediateNegativesGenerator extends ForestFireLabelledStateMachineGenerator {

	public ForestFireIntermediateNegativesGenerator(double forwards,double backwards, double argParallel, int alphabetSize, int seed) 
	{
		super(forwards, backwards, argParallel,  alphabetSize, seed);
	}
	


	@Override
	protected void annotateVertex(DeterministicVertex vertex)
	{
		super.annotateVertex(vertex);
		vertex.setAccept(randomInt(2) > 0);
	}

	
	public static void main(String args[])
	{
		for(double f=0.1;f < 1;f+=0.05)
		{
			final int nr = 10;
			double accumMachine = 0, accumReduced = 0, statesMachine = 0, statesReduced = 0;
			for(int counter=0;counter < nr;++counter)
			{
				ForestFireIntermediateNegativesGenerator gen = new ForestFireIntermediateNegativesGenerator(f,0.4,0.2,10,0);
				LearnerGraph fsm = gen.buildMachine(130);
				accumMachine += ForestFireStateMachineGenerator.getEffectiveDiameter(gen.machine);statesMachine+=gen.machine.getVertices().size();
				accumReduced += ForestFireStateMachineGenerator.getEffectiveDiameter(fsm.pathroutines.getGraph());statesReduced+=fsm.getStateNumber();

			}
			System.out.println(f+" effective dim orig: "+accumMachine/nr+" effective dim reduced: "+accumReduced/nr+" statesOrig: "+statesMachine/nr+" statesReduced: "+statesReduced/nr);
		}
		
		/* Illustration */
		ForestFireIntermediateNegativesGenerator gen = new ForestFireIntermediateNegativesGenerator(0.5,0.4,0.2,10,0);
		LearnerGraph fsm = gen.buildMachine(13);

		System.out.println(fsm.pathroutines.computeAlphabet().size() + ", "+fsm.getStateNumber());
		//Visualiser.updateFrame(gen.machine, fsm);
		//Visualiser.waitForKey();
	}
}
