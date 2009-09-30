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

import java.util.HashSet;
import java.util.Set;

import cern.jet.random.Distributions;

import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import edu.uci.ics.jung.utils.UserData;

/**
 * Adds an alphabet of a specific size, ensuring that the final machine is deterministic.
 * Also adds the potential of loops to the same state.
 * 
 * @author nw
 *
 */
public class ForestFireLabelledStateMachineGenerator extends ForestFireStateMachineGenerator 
{
	Set<String> alphabet;
	double parallel;
	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, double argParallel, int alphabetSize, int seed)
	{
		super(forwards, backwards,seed);this.parallel=argParallel;
		this.alphabet=generateAlphabet(alphabetSize);
	}
	
	public ForestFireLabelledStateMachineGenerator(double forwards, double backwards, double argParallel, Set<String> argAlphabet, int seed)
	{
		super(forwards, backwards, seed);this.parallel=(int)(1/argParallel);
		this.alphabet=argAlphabet;
	}
	
	@Override
	protected boolean addEdge(DeterministicVertex v, DeterministicVertex random)
	{
		int numberOfLabels = Distributions.nextGeometric(1-parallel,generator);
		if (!addEdgeInternal(v,random)) 
			return false;
		for(int i=0;i<numberOfLabels && addEdgeInternal(v,random);++i);
		return true;			
	}
	
	protected boolean addEdgeInternal(DeterministicVertex v, DeterministicVertex random)
	{
		Set<String> vertexAlphabet = new HashSet<String>();
		DirectedSparseEdge existingEdge = null;
		for (Object e : v.getOutEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			if (edge.getDest() == random)
				existingEdge = edge;
			Set<String>labels = (Set<String>)edge.getUserDatum(JUConstants.LABEL);
			assert labels!=null : "vertex "+v.getID().toString()+" has outgoing edges without labels";
			vertexAlphabet.addAll(labels);
		}
		Set<String> possibles = new HashSet<String>();
		possibles.addAll(alphabet);
		possibles.removeAll(vertexAlphabet);
		String label = null;
		if(possibles.isEmpty())
			return false;// failure to add an edge since all possible letters of an alphabet have already been used
		String possiblesArray [] = new String[possibles.size()];possibles.toArray(possiblesArray); 
		label = possiblesArray[randomInt(possiblesArray.length)];
		
		if (existingEdge != null)
		{// a parallel edge
			((Set<String>)existingEdge.getUserDatum(JUConstants.LABEL)).add(label);
		}
		else
		{// new edge needs to be added.
			try
			{
				Set<String> labelSet = new HashSet<String>();
				labelSet.add(label);
				DirectedSparseEdge e = new DirectedSparseEdge(v,random);
				e.addUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);
				machine.addEdge(e);
			}
			catch(edu.uci.ics.jung.exceptions.ConstraintViolationException e1){
				Helper.throwUnchecked("poor constraints from"+v+" to "+random,e1);
			}
		}
		return true;
	}
	
	private static Set<String> generateAlphabet(int number)
	{
		Set<String> alphabet = new HashSet<String>();
		for (int i=0;i<number;i++){
			String next = String.valueOf(i);
			alphabet.add(next);
		}
		return alphabet;
	}
	
}
