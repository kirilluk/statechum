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
package statechum.analysis.learning.util;

import java.io.File;
import java.io.IOException;
import java.util.*;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.StringVertex;
import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.linear.GDLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.PairScoreComputation;
import statechum.analysis.learning.linear.GDLearnerGraph.StateBasedRandom;


public class ScoreMatrixOutput {
	
	public static void main(String[] args) throws IOException
	{
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraph dsg = FsmParser.buildLearnerGraph("q0-initialise->q1-connect->q2-login->q3-setfiletype->q4-rename->q6-storefile->q5-setfiletype->q4-storefile->q7-appendfile->q5-setfiletype->q4\nq3-makedir->q8-makedir->q8-logout->q16-disconnect->q17\nq3-changedir->q9-listnames->q10-delete->q10-changedir->q9\nq10-appendfile->q11-logout->q16\nq3-storefile->q11\nq3-listfiles->q13-retrievefile->q13-logout->q16\nq13-changedir->q14-listfiles->q13\nq7-logout->q16\nq6-logout->q16", "specgraph",config,null);
		LearnerGraph gr=new LearnerGraph(dsg,config);
		writeMatrix(gr, "cvsExample.csv");
	}
	
	
	public static void writeMatrix(LearnerGraph gr, String name) throws IOException
	{
		Stack<PairScore> pairScores = PairScoreComputation.chooseStatePairs(gr,GDLearnerGraph.PAIR_INCOMPATIBLE*2,10,1,null,LearnerGraphND.ignoreNone, new StateBasedRandom(1));
		ArrayList<StringVertex> vertexList = new ArrayList<StringVertex>();
		DirectedSparseGraph dsg = gr.pathroutines.getGraph();
		vertexList.addAll(dsg.getVertices());
		writeMatrix(pairScores,vertexList,vertexList, name);
	}

	private static void writeMatrix(List<PairScore> ps, List<StringVertex> stateLabelsA, List<StringVertex> stateLabelsB, String name) throws IOException{
		String fileRef = statechum.GlobalConfiguration.getConfiguration().getProperty(statechum.GlobalConfiguration.G_PROPERTIES.TEMP)+File.separator+name;
		File output  = new File(fileRef);
		long[][] matrix = new long[stateLabelsA.size()][stateLabelsB.size()];
		//int max = 0;
		for (PairScore p : ps) {
			int indexA = stateLabelsA.indexOf(p.getQ());
			int indexB = stateLabelsB.indexOf(p.getR());
			//int score = p.getScore();
			matrix[indexA][indexB] = p.getScore();
			matrix[indexB][indexA] = p.getScore();
			
		}
		String matrixString = matrixToString(matrix,stateLabelsA, stateLabelsB);
		OutputUtil.write(matrixString, output);
	}
	
	private static String matrixToString(long[][] matrix, List<StringVertex>stateLabelsA, List<StringVertex>stateLabelsB){
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
