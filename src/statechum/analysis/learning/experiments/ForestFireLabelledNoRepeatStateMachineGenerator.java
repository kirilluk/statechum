/* Copyright (c) 2016 The University of Sheffield
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

import edu.uci.ics.jung.graph.impl.DirectedSparseEdge;
import statechum.Configuration;
import statechum.JUConstants;
import statechum.Label;
import statechum.DeterministicDirectedSparseGraph.DeterministicVertex;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class ForestFireLabelledNoRepeatStateMachineGenerator extends ForestFireLabelledStateMachineGenerator 
{

	public ForestFireLabelledNoRepeatStateMachineGenerator(double forwards, double backwards, double argSelfloop,
			double argParallel, int alphabetSize, int seed, Configuration conf, ConvertALabel converter) {
		super(forwards, backwards, argSelfloop, argParallel, alphabetSize, seed, conf, converter);
	}

	/** Makes it possible for derived classes to use different patterns for selection of plausible labels. */
	@SuppressWarnings("unchecked")
	@Override
	protected Label selectLabel(DeterministicVertex v, Set<Label> outgoingAlphabet)
	{
		Set<Label> incomingAlphabet = new TreeSet<Label>();
		for(Object e:v.getInEdges()) {
			DirectedSparseEdge edge = (DirectedSparseEdge)e;
			incomingAlphabet.addAll((Set<Label>)edge.getUserDatum(JUConstants.LABEL));
		}

		Set<Label> possibles = new TreeSet<Label>();
		possibles.addAll(alphabet);
		possibles.removeAll(outgoingAlphabet);possibles.removeAll(incomingAlphabet);
		if(possibles.isEmpty())
			return null;
		
		Label possiblesArray [] = new Label[possibles.size()];possibles.toArray(possiblesArray);
		return possiblesArray[randomInt(possiblesArray.length)];
	}
	
	/** Overridden to stop generation of self-loops. */
	@Override
	protected boolean addEdge(DeterministicVertex v, DeterministicVertex random)
	{
		if (v == random)
			return false;
		return super.addEdge(v, random);
	}
}
