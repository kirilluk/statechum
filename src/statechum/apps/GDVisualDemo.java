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
import statechum.GlobalConfiguration;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.analysis.Erlang.Synapse;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.linear.DifferenceVisualiser.ChangesToGraph;
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
	public static DirectedSparseGraph obtainDifferenceGraph(String graphA, String graphB,int counter,boolean display, Configuration config)
	{
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();

		LearnerGraphND grA=buildLearnerGraphND(graphA, "labellingDemo_A_"+counter,config,null),
				grB=buildLearnerGraphND(graphB, "labellingDemo_B_"+counter,config,null);
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
	
	
	public static void obtainDifferenceGraph2(String graphA, String graphB,int counter,boolean display, Configuration config)
	{
		LearnerGraphND grA=buildLearnerGraphND(graphA, "labellingDemo_A_"+counter,config,null),
				grB=buildLearnerGraphND(graphB, "labellingDemo_B_"+counter,config,null);
		DirectedSparseGraph gr = ChangesToGraph.computeVisualisationParameters(Synapse.StatechumProcess.constructFSM(grA), ChangesToGraph.computeGD(grA, grB,config));
		if (display)
		{
			Visualiser.updateFrame(gr, null);
		}
		
	}	
	
	public static void main(String str[])
	{// -ea -Xmx1600m -Xms800m -XX:NewRatio=1 -XX:+UseParallelGC -Dthreadnum=2 -DVIZ_CONFIG=kirill_tmp
		GlobalConfiguration.getConfiguration().setProperty(G_PROPERTIES.CLOSE_TERMINATE,"true");
		@SuppressWarnings("unchecked")
		Pair<String,String> [] pairs=new Pair[] {
				/*
				new Pair<String,String>(
						"A-initialise->B-connect->C-login->D / "+
						"D-changedirectory->E-listnames->F-delete->F-changedirectory->E / F-appendfile->G-logout->H-disconnect->I / D-storefile->G /"+ 
						"D-listfiles->J-retrievefile->J-changedirectory->K-listfiles->J-logout->H / D-makedir->M-makedir->M-logout->H / "+
						"D-setfiletype->N-rename->O-logout->H / O-storefile->P-setfiletype->N-storefile->Q-logout->H / Q-appendfile->P",
						//"A-initialise->B-connect->C-login->D-setfiletype->D-retrievefile->D-delete->D-storefile->D-makedir->D-listfiles->D-changedirectory->E-listnames->D\nE-listfiles->D-appendfile->F-setfiletype->H-rename->D-rename->F-logout->G-disconnect->I\nD-logout->G",
						
						//"a-initialise->b-connect->c-login->d-storefile->e-changedirectory->d-listfiles->e-retrievefile->e-logout->i-disconnect->l\nd-delete->d-makedir->d-changedirectory->f-listnames->d-logout->g-disconnect->j\nd-setfiletype->h-storefile->k-appendfile->m-setfiletype->n-rename->o-storefile->m\nd-appendfile->o-logout->p-disconnect->q",
						
						"a-initialise->d-connect->f-login->h-storefile->m\nh-listfiles->l-retrievefile->l-changedirectory->p-listfiles->l-logout->o-disconnect->q\nh-changedirectory->n-listnames->i-delete->i-delete->j-changedirectory->n\ni-appendfile->k-logout->o\nb-setfiletype->e-rename->g\nc-makedir->c"

						),*/
				new Pair<String,String>("A-a->B-a->C / A-c->B / A-e->B / A-b->D", "P-a->Q-a->R / P-c->Q / P-d->Q / S-c->R"),
			//new Pair<String,String>("A-a->A-a->C / A-c->A / A-e->A / A-b->D", "P-a->P-a->R / P-c->P / P-d->P / S-c->R"),
				//new Pair<String,String>("A-a->B-a->C-b->D", "P-a->Q-b->R-c->R"),
				
				//new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->S","A-a->C-c->E-c->D-c->F-s->T\nA-d->T-d->T"),
				//new Pair<String,String>("A-a->B-a->C\nB-b->D\nA-d->T-d->T","A-a->C-c->E-c->D-c->F-s->T\nA-d->U-d->U"),
				//new Pair<String,String>(TestGD_Multithreaded.A6,TestGD_Multithreaded.B6)
				//new Pair<String,String>("A-a->B-a-#C / A-b->D", "P-a->Q-a-#R / S-c-#R"),
				};
		int position=0;
		GlobalConfiguration.getConfiguration().setProperty(GlobalConfiguration.G_PROPERTIES.ESC_TERMINATE,"false");
		obtainDifferenceGraph2(pairs[position].firstElem, pairs[position].secondElem, position,true, Configuration.getDefaultConfiguration());
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		obtainDifferenceGraph2(pairs[position].firstElem, pairs[position].secondElem, position,true, Configuration.getDefaultConfiguration());
		
	}
}
