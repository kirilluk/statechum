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

package statechum.model.testset;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class PTA_FSMStructure extends PTASequenceEngine 
{
	public PTA_FSMStructure(final LearnerGraph machine, final CmpVertex initState) 
	{
		//if (initState != null && machine.findVertex(initState.getID()) != initState)
		// verifies that the supplied state is a valid one.
		//throw new IllegalArgumentException("state "+initState+" is not a valid state of the graph");
		if (initState != null) machine.verifyVertexInGraph(initState);
		
		init(machine.new FSMImplementation() {
			@Override
			public Object getInitState() {
				return initState == null? machine.getInit():initState;
			} 
			@Override
			public boolean shouldBeReturned(@SuppressWarnings("unused")	Object elem) 
			{
				return true;
			}
			
		});
	}
}
