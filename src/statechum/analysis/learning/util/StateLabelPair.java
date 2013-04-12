/**
 * 
 */
package statechum.analysis.learning.util;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;

class StateLabelPair{
	CmpVertex vertex=null;
	Label label = null;
	
	public StateLabelPair(CmpVertex vertexArg, Label labelArg) {
		this.vertex = vertexArg;
		this.label=labelArg;
	}
	
}