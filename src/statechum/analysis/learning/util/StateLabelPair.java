/**
 * 
 */
package statechum.analysis.learning.util;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;

class StateLabelPair{
	CmpVertex vertex=null;
	Label label = null;
	
	public StateLabelPair(CmpVertex vertex, Label label) {
		this.vertex = vertex;
		this.label=label;
	}
	
}