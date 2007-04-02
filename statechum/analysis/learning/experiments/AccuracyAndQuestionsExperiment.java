/*
 * INCOMPLETE
 */

package statechum.analysis.learning.experiments;
import java.util.*;
import edu.uci.ics.jung.graph.impl.*;
import edu.uci.ics.jung.graph.*;
import statechum.xmachine.model.testset.*;

public class AccuracyAndQuestionsExperiment {
	
	public void evaluate(DirectedSparseGraph g){
		WMethod wm = new WMethod(g,0);
		Vector fullTestSet = wm.getFullTestSet();
		String fsmString = getFSMString(g);
		for(int i=10;i<=100;i=i+10){
			Vector samples = randomHalf(fullTestSet);
			
		}
	}
	
	private String getFSMString(DirectedSparseGraph g){
		String fsmString = "";
		for(DirectedSparseEdge e:(Collection<DirectedSparseEdge>)g.getEdges()){
			String sourceLabel = e.getSource().getUserDatum("VERTEX").toString();
			String targetLabel = e.getDest().getUserDatum("VERTEX").toString();
			String edgeLabel = e.getUserDatum("EDGE").toString();
			if(sourceLabel.startsWith("Initial"))
				fsmString = ("\\n"+sourceLabel+"-"+edgeLabel+"->"+targetLabel).concat(fsmString);
			else
				fsmString = fsmString.concat("\\n"+sourceLabel+"-"+edgeLabel+"->"+targetLabel);
		}
		return fsmString;
	}
	
	private Vector randomHalf(Vector v){
		Vector returnVect = new Vector();
		Random generator = new Random();
		for(int i=0;i<v.size()/2;i++){
			returnVect.add(v.get(generator.nextInt(v.size())));
		}
		return returnVect;
	}
	
}
