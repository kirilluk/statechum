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
package statechum.apps;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraphND;

import java.util.List;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;

/** Demonstrates how vertices and edges can be labelled.
 * 
 * @author kirr
 *
 */
public class GDVisualDemo
{
	public static DirectedSparseGraph obtainDifferenceGraph(String graphA, String graphB,int counter,boolean display)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		Configuration config = Configuration.getDefaultConfiguration();
		LearnerGraphND grA=buildLearnerGraphND(graphA, "labellingDemo_A_"+counter,config),grB=
		buildLearnerGraphND(graphB, "labellingDemo_B_"+counter,config);
		DirectedSparseGraph gr = gd.showGD(
				grA,grB,
				ExperimentRunner.getCpuNumber());
		if (display)
		{
		Visualiser.updateFrame(grA, grB);
		Visualiser.updateFrame(gr, null);
		}
		return gr;
	}
	
	public static void main(String str[])
	{// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
	
		Pair<String,String> [] pairs=new Pair[] {
				new Pair<String,String>(
						"A-initialise->B-connect->C-login->D / "+
						"D-changedirectory->E-listnames->F-delete->F-changedirectory->E / F-appendfile->G-logout->H-disconnect->I / D-storefile->G /"+ 
						"D-listfiles->J-retrievefile->J-changedirectory->K-listfiles->J-logout->H / D-makedir->M-makedir->M-logout->H / "+
						"D-setfiletype->N-rename->O-logout->H / O-storefile->P-setfiletype->N-storefile->Q-logout->H / Q-appendfile->P",
						//"A-initialise->B-connect->C-login->D-setfiletype->D-retrievefile->D-delete->D-storefile->D-makedir->D-listfiles->D-changedirectory->E-listnames->D\nE-listfiles->D-appendfile->F-setfiletype->H-rename->D-rename->F-logout->G-disconnect->I\nD-logout->G",
						
						//"a-initialise->b-connect->c-login->d-storefile->e-changedirectory->d-listfiles->e-retrievefile->e-logout->i-disconnect->l\nd-delete->d-makedir->d-changedirectory->f-listnames->d-logout->g-disconnect->j\nd-setfiletype->h-storefile->k-appendfile->m-setfiletype->n-rename->o-storefile->m\nd-appendfile->o-logout->p-disconnect->q",
						/*"P1001 - initialise-> P1002 /"+
						"P1005 - retrievefile-> P1005 /"+
						"P1026 - listnames-> P1004 /"+
						"P1030 - disconnect-> P1031 /"+
						"P1004 - appendfile-> P1029 /"+
						"P1005 - changedirectory-> P1004 /"+
						"P1004 - delete-> P1004 /"+
						"P1004 - makedir-> P1004 /"+
						"P1029 - storefile-> P1053 /"+
						"P1002 - connect-> P1003 /"+
						"P1053 - setfiletype-> P1054 /"+
						"P1004 - storefile-> P1005 /"+
						"P1004 - listfiles-> P1005 /"+
						"P1054 - rename-> P1029 /"+
						"P1052 - appendfile-> P1053 /"+
						"P1003 - login-> P1004 /"+
						"P1004 - setfiletype-> P1051 /"+
						"P1051 - storefile-> P1052 /"+
						"P1006 - disconnect-> P1007 /"+
						"P1004 - logout-> P1093 /"+
						"P1005 - logout-> P1006 /"+
						"P1029 - logout-> P1030 /"+
						"P1093 - disconnect-> P1094 /"+
						"P1004 - changedirectory-> P1026",*/
						"a-initialise->d-connect->f-login->h-storefile->m\nh-listfiles->l-retrievefile->l-changedirectory->p-listfiles->l-logout->o-disconnect->q\nh-changedirectory->n-listnames->i-delete->i-delete->j-changedirectory->n\ni-appendfile->k-logout->o" //\nb-setfiletype->e-rename->g\nc-makedir->c"

						),
				new Pair<String,String>("A-a->B-a->C / A-b->D", "P-a->Q-a->R / S-c->R"),
				new Pair<String,String>("A-a->B-a->C-b->D", "P-a->Q-b->R-c->R"),
				//new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->S","A-a->C-c->E-c->D-c->F-s->T\nA-d->T-d->T"),
				//new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->T","A-a->C-c->E-c->D-c->F-s->T\nA-d->U-d->U"),
				//new Pair<String,String>(TestGD_Multithreaded.A6,TestGD_Multithreaded.B6)
				};
		int position=0;
		obtainDifferenceGraph(pairs[position].firstElem, pairs[position].secondElem, position,true);
		
	}
}
