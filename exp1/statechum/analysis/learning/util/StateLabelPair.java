/**
 * 
 */
package statechum.analysis.learning.util;

import statechum.DeterministicDirectedSparseGraph.CmpVertex;

class StateLabelPair{
	CmpVertex vertex=null;
	String label = null;
	
	public StateLabelPair(CmpVertex vertex, String label) {
		this.vertex = vertex;
		this.label=label;
	}
	
}