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

import java.util.Set;
import java.util.TreeSet;

import cern.jet.random.Distributions;

import statechum.Configuration;
import statechum.Helper;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.DeterministicEdge;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.Label;

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
	Set<Label> alphabet;
	double parallel;
	public ForestFireLabelledStateMachineGenerator(
			double forwards, double backwards, double argSelfloop, double argParallel, int alphabetSize, 
			int seed,Configuration conf,ConvertALabel converter)
	{
		super(forwards, backwards,argSelfloop,seed,conf,converter);this.parallel=(int)(1/argParallel);
		this.alphabet=generateAlphabet(alphabetSize);
	}
	
	public ForestFireLabelledStateMachineGenerator(
			double forwards, double backwards, double argSelfloop, double argParallel, Set<Label> argAlphabet, 
			int seed,Configuration conf,ConvertALabel converter)
	{
		super(forwards, backwards, argSelfloop, seed,conf,converter);this.parallel=(int)(1/argParallel);
		this.alphabet=argAlphabet;
	}
	
	@Override
	protected boolean addEdge(DeterministicVertex v, DeterministicVertex random)
	{
		int numberOfLabels = Distributions.nextGeometric(1-parallel,generator);
		if (!addEdgeInternal(v,random)) 
			return false;
		if (randomInt(2) > 0)
		{
			for(int i=0;i<numberOfLabels && addEdgeInternal(random,v);++i)
			{}
		}
		else
		{
			for(int i=0;i<numberOfLabels && addEdgeInternal(v,random);++i)
			{}
		}
		return true;			
	}
	
	/** Makes it possible for derived classes to use different patterns for selection of plausible labels. */
	protected Label selectLabel(@SuppressWarnings("unused") DeterministicVertex v, Set<Label> outgoingAlphabet)
	{
        Set<Label> possibleLabels = new TreeSet<>(alphabet);
		possibleLabels.removeAll(outgoingAlphabet);
		if(possibleLabels.isEmpty())
			return null;
		
		Label possiblesArray [] = new Label[possibleLabels.size()];possibleLabels.toArray(possiblesArray);
		return possiblesArray[randomInt(possiblesArray.length)];
	}
	
	@SuppressWarnings("unchecked")
	protected boolean addEdgeInternal(DeterministicVertex v, DeterministicVertex random)
	{
		Set<Label> vertexAlphabet = new TreeSet<>();
		DirectedSparseEdge existingEdge = null;
		
		for (Object e : v.getOutEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			if (edge.getDest() == random)
				existingEdge = edge;
			Set<Label>labels = (Set<Label>)edge.getUserDatum(JUConstants.LABEL);
			assert labels!=null : "vertex "+v.getStringId()+" has outgoing edges without labels";
			vertexAlphabet.addAll(labels);
		}

		Label label = selectLabel(v,vertexAlphabet);
		if(label == null)
			return false;// failure to add an edge since all possible letters of an alphabet have already been used
		
		if (existingEdge != null)
		{// a parallel edge
			((Set<Label>)existingEdge.getUserDatum(JUConstants.LABEL)).add(label);
		}
		else
		{// new edge needs to be added.
			try
			{
				Set<Label> labelSet = new TreeSet<>();
				labelSet.add(label);
				DeterministicEdge e = AbstractLearnerGraph.generateNewJungEdge(v,random);
				e.addUserDatum(JUConstants.LABEL, labelSet, UserData.SHARED);
				machine.addEdge(e);
			}
			catch(edu.uci.ics.jung.exceptions.ConstraintViolationException e1){
				Helper.throwUnchecked("poor constraints from"+v+" to "+random,e1);
			}
		}
		return true;
	}
	
	private Set<Label> generateAlphabet(int number)
	{
		Set<Label> generatedAlphabet = new TreeSet<>();
		for (int i=0;i<number;i++)
			generatedAlphabet.add(AbstractLearnerGraph.generateNewLabel(i, config, converter));
		return generatedAlphabet;
	}
	
}
