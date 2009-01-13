package statechum.analysis.learning.util;

import java.io.File;
import java.util.*;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.StringVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.GDLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;


public class ScoreMatrixOutput {
	
	public static void main(String[] args){
		Configuration config = Configuration.getDefaultConfiguration();
		DirectedSparseGraph dsg = TestFSMAlgo.buildGraph("q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5-setfiletype->q4\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedir->q9-listnames->q10-delete->q10-changedir->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedir->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "specgraph");
		LearnerGraph gr=new LearnerGraph(dsg,config);
		writeMatrix(gr, "cvsExample.csv");
	}
	
	
	public static void writeMatrix(LearnerGraph gr, String name){
		Stack<PairScore> pairScores = gr.pairscores.chooseStatePairs(GDLearnerGraph.PAIR_INCOMPATIBLE*2,10,1,null,LearnerGraphND.ignoreNone);
		ArrayList vertexList = new ArrayList();
		DirectedSparseGraph dsg = gr.pathroutines.getGraph();
		vertexList.addAll(dsg.getVertices());
		writeMatrix(pairScores,vertexList,vertexList, name);
	}

	private static void writeMatrix(List<PairScore> ps, List<StringVertex> stateLabelsA, List<StringVertex> stateLabelsB, String name){
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File output  = new File(fileRef);
		int[][] matrix = new int[stateLabelsA.size()][stateLabelsB.size()];
		int max = 0;
		for (PairScore p : ps) {
			int indexA = stateLabelsA.indexOf(p.getQ());
			int indexB = stateLabelsB.indexOf(p.getR());
			int score = p.getScore();
			matrix[indexA][indexB] = p.getScore();
			matrix[indexB][indexA] = p.getScore();
			
		}
		String matrixString = matrixToString(matrix,stateLabelsA, stateLabelsB);
		OutputUtil.write(matrixString, output);
	}
	
	private static String matrixToString(int[][] matrix, List<StringVertex>stateLabelsA, List<StringVertex>stateLabelsB){
		String s = ",";
		for(int i=0;i<stateLabelsB.size();i++){
			if(i<stateLabelsB.size()-1)
				s = s+stateLabelsB.get(i)+",";
			else
				s = s+stateLabelsB.get(i);
		}
		s = s + "\n";
		for(int i=0;i<stateLabelsA.size();i++){
			s = s + stateLabelsA.get(i)+",";
			for(int j = 0;j<stateLabelsB.size();j++){
				if(j<stateLabelsB.size()-1)
					s = s.concat(String.valueOf(matrix[i][j]))+",";
				else
					s = s.concat(String.valueOf(matrix[i][j]));
			}
			s = s.concat("\n");
		}
		return s;
	}

}
