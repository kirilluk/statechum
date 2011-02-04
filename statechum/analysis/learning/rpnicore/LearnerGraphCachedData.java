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

package statechum.analysis.learning.rpnicore;

import java.util.Collection;
import java.util.Map;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.rpnicore.LabelRepresentation.AbstractState;
import statechum.analysis.learning.rpnicore.LabelRepresentation.Label;

/** An extension of the cache with elements to support learning. */ 
public class LearnerGraphCachedData extends CachedData<CmpVertex,LearnerGraphCachedData>
{
	LearnerGraphCachedData(LearnerGraph g) {
		super(g);
	}

	/** After merging using mergeAndDeterminize_general,
	 * this variable stores equivalence classes. Used by the pluggable
	 * question generator.
	 */ 
	protected Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> mergedStates = null;
	
	public Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> getMergedStates()
	{
		return mergedStates;
	}
	
	public void setMergedStates(Collection<AMEquivalenceClass<CmpVertex,LearnerGraphCachedData>> eqClasses)
	{
		mergedStates = eqClasses;
	}

        /** A collection associating merged states to hard facts-states they came from. */
        protected Map<VertexID,Collection<VertexID>> mergedToHardFacts;

        public Map<VertexID,Collection<VertexID>> getMergedToHardFacts()
        {
            return mergedToHardFacts;
        }

	/** The maximal score which can be returned by score computation routines. 
	 */
	protected int maxScore = -1;
	
	/** The state corresponding to the red and blue states after the merge of which this graph was built. */
	protected CmpVertex stateLearnt = null;
	
	@Override
	public void invalidate()
	{
		super.invalidate();
		mergedStates = null;maxScore=-1;stateLearnt = null;
		abstractStateToLabelPreviouslyChecked = null;
	}
	
	/** A map associating an abstract state and a set of transitions which have been evaluated from 
	 * that state using TRANSITIONSFROMALLORNONE.
	 * <p>
	 * Since the process of merging is additive, 
	 * <ul>
	 * <li>if there are two abstract states (previously associated to different DFA states) from which
	 * transitions <em>a</em> and <em>b</em> have been checked, and subsequently these states are merged
	 * we only need to check <em>b</em> from the first state and <em>a</em> from the second one.</li>
	 * <li>
	 * If the outcome of <em>a</em> is in one of the abstract states associated with its target DFA state, then
	 * after the merge there would be not fewer abstract states associated with its target DFA state, thus 
	 * the range intersection condition will be satisfied.</li>
	 * </ul>
	 */
	Map<Label,Collection<AbstractState>> abstractStateToLabelPreviouslyChecked = null;
	
	public Map<Label,Collection<AbstractState>> getAbstractStateToLabelPreviouslyChecked()
	{
		return abstractStateToLabelPreviouslyChecked;
	}
}
