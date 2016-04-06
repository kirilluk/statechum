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

import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Configuration.STATETREE;
import statechum.JUConstants;
import statechum.Pair;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.rpnicore.AbstractPathRoutines;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.PathRoutines.EdgeAnnotation;
import statechum.analysis.learning.linear.GD;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.LearnerGraphNDCachedData;
import statechum.analysis.learning.linear.TestGD_Multithreaded;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestVisualDemo {
	
	private static Pair<String,String> differenceLabelling(String graphA,String graphB)
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		DirectedSparseGraph graph = GDVisualDemo.obtainDifferenceGraph(graphA,graphB, 0,false,config);
		@SuppressWarnings("unchecked")
		Map<String,String> labelling = (Map<String,String>)graph.getUserDatum(JUConstants.VERTEX);
		final EdgeAnnotation transitionAnnotation = (EdgeAnnotation)graph.getUserDatum(JUConstants.EDGE);
		//Visualiser.updateFrame(graph, null);Visualiser.waitForKey();
		return new Pair<String,String>(labelling.toString(),transitionAnnotation.toString());
	}
	
	@Test
	public final void testVisual1()
	{
		Pair<String,String> pair = differenceLabelling("A-a->B-a->C\nB-b->D","A-a->C-c->E-c->D-c->F");
		Assert.assertEquals("{A=(K 5,0=A), B=DEL, C=(K 5,0=C), D=KEPT, E=ADD, F=ADD}",pair.firstElem);
		Assert.assertEquals("{A={ADD_a={C=java.awt.Color[r=0,g=255,b=0]}, "+
				"REM_a={B=java.awt.Color[r=255,g=0,b=0]}}, "+
				"B={REM_a={C=java.awt.Color[r=255,g=0,b=0]}, "+
				"REM_b={D=java.awt.Color[r=255,g=0,b=0]}}, "+
				"C={ADD_c={E=java.awt.Color[r=0,g=255,b=0]}}, "+
				"D={ADD_c={F=java.awt.Color[r=0,g=255,b=0]}}, "+
				"E={ADD_c={D=java.awt.Color[r=0,g=255,b=0]}}}",pair.secondElem);
	}
	
	@Test
	public final void testVisual2()
	{
		Pair<String,String> pair = differenceLabelling("A-a->B-a->C\nB-b->D\nA-d->T-d->S","A-a->C-c->E-c->D-c->F-s->T\nA-d->T-d->T");
		Assert.assertEquals("{A=(P=A,0.575), B=(P=C,0.5)[C] , C=DEL, D=KEPT, E=ADD, F=ADD, S=DEL, T=(K 8,2=T)}",pair.firstElem);
		Assert.assertEquals("{A={a={B=java.awt.Color[r=0,g=0,b=0]}, d={T=java.awt.Color[r=0,g=0,b=0]}}, B={ADD_c={E=java.awt.Color[r=0,g=255,b=0]}, REM_a={C=java.awt.Color[r=255,g=0,b=0]}, REM_b={D=java.awt.Color[r=255,g=0,b=0]}}, "+
				"D={ADD_c={F=java.awt.Color[r=0,g=255,b=0]}}, "+
				"E={ADD_c={D=java.awt.Color[r=0,g=255,b=0]}}, "+
				"F={ADD_s={T=java.awt.Color[r=0,g=255,b=0]}}, "+
				"T={ADD_d={T=java.awt.Color[r=0,g=255,b=0]}, REM_d={S=java.awt.Color[r=255,g=0,b=0]}}}",pair.secondElem);
	}
	
	@Test
	public final void testVisual3()
	{
		Pair<String,String> pair = differenceLabelling("A-a->B-a->C\nB-b->D\nA-d->T-d->T","A-a->C-c->E-c->D-c->F-s->T\nA-d->U-d->U");
		Assert.assertEquals("{A=(P=A,0.6071428571428572), B=(P=C,0.5)[C] , C=DEL, D=KEPT, E=ADD, F=ADD, P1005=DUP[T] , T=(K 12,3=U)[U] }",pair.firstElem);
		Assert.assertEquals("{A={a={B=java.awt.Color[r=0,g=0,b=0]}, d={T=java.awt.Color[r=0,g=0,b=0]}}, B={ADD_c={E=java.awt.Color[r=0,g=255,b=0]}, REM_a={C=java.awt.Color[r=255,g=0,b=0]}, REM_b={D=java.awt.Color[r=255,g=0,b=0]}}, "+
				"D={ADD_c={F=java.awt.Color[r=0,g=255,b=0]}}, "+
				"E={ADD_c={D=java.awt.Color[r=0,g=255,b=0]}}, "+
				"F={ADD_s={P1005=java.awt.Color[r=0,g=255,b=0]}}, T={d={T=java.awt.Color[r=0,g=0,b=0]}}}",pair.secondElem);
	}
	
	@Test
	public final void testVisual4()
	{
		Pair<String,String> pair = differenceLabelling(TestGD_Multithreaded.A6,TestGD_Multithreaded.B6);
		Assert.assertEquals("{A=(P=B@,1.4285714285714286)[B@] , B=DEL, C=(P=C@,1.4285714285714286)[C@] , D=(K 14,14=A@)[A@] , E=(P=F@,1.3127413127413128)[F@] , F=DEL, G=(P=G@,1.3127413127413128)[G@] , H=(P=E@,1.3127413127413128)[E@] , I=(P=J@,1.30334968172806)[J@] , J=DEL, K=(P=K@,1.30334968172806)[K@] , L=(P=I@,1.30334968172806)[I@] }",pair.firstElem);
		Assert.assertEquals("{A={ADD_a={C=java.awt.Color[r=0,g=255,b=0]}, REM_a={B=java.awt.Color[r=255,g=0,b=0]}, b={E=java.awt.Color[r=0,g=0,b=0]}}, B={REM_a={C=java.awt.Color[r=255,g=0,b=0]}, REM_b={F=java.awt.Color[r=255,g=0,b=0]}}, C={a={D=java.awt.Color[r=0,g=0,b=0]}, b={G=java.awt.Color[r=0,g=0,b=0]}}, D={a={A=java.awt.Color[r=0,g=0,b=0]}, b={H=java.awt.Color[r=0,g=0,b=0]}}, "+
				"E={ADD_b={G=java.awt.Color[r=0,g=255,b=0]}, REM_b={F=java.awt.Color[r=255,g=0,b=0]}, a={I=java.awt.Color[r=0,g=0,b=0]}}, "+
				"F={REM_a={J=java.awt.Color[r=255,g=0,b=0]}, REM_b={G=java.awt.Color[r=255,g=0,b=0]}}, "+
				"G={a={K=java.awt.Color[r=0,g=0,b=0]}, b={H=java.awt.Color[r=0,g=0,b=0]}}, H={a={L=java.awt.Color[r=0,g=0,b=0]}, b={E=java.awt.Color[r=0,g=0,b=0]}}, "+
				"I={ADD_a={K=java.awt.Color[r=0,g=255,b=0]}, REM_a={J=java.awt.Color[r=255,g=0,b=0]}}, "+
				"J={REM_a={K=java.awt.Color[r=255,g=0,b=0]}}, "+
				"K={a={L=java.awt.Color[r=0,g=0,b=0]}}, "+
				"L={a={I=java.awt.Color[r=0,g=0,b=0]}}"+
				"}",pair.secondElem);
	}
	
	/** Disconnected states. */
	@SuppressWarnings("unchecked")
	@Test
	public final void testVisual5a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		config.setGdKeyPairThreshold(1);config.setGdLowToHighRatio(1);
		String name = "testVisual5";
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nS-a-#T";
		LearnerGraphND grA = FsmParser.buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nU-a-#V\n"+common,name+"A",config,null);
		LearnerGraphND grB = FsmParser.buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",config,null);
		LearnerGraphND grA_reduced = new LearnerGraphND(config), grB_reduced = new LearnerGraphND(config);
		AbstractPathRoutines.removeRejectStates(grA, grA_reduced);
		AbstractPathRoutines.removeRejectStates(grB, grB_reduced);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		DirectedSparseGraph graph = gd.showGD(grA_reduced,grB_reduced,	ExperimentRunner.getCpuNumber());
		Assert.assertEquals("{A=(K 5,0=A), B=(K 13,1=B), C=(K 13,1=C), D=(K 13,1=D), E=(P=G,0.16666666666666666)[G] , F=(P=H,0.16666666666666666)[H] , S=KEPT, U=DEL}",((Map<String,String>)graph.getUserDatum(JUConstants.VERTEX)).toString());
		Assert.assertEquals("{A={a={B=java.awt.Color[r=0,g=0,b=0], C=java.awt.Color[r=0,g=0,b=0], D=java.awt.Color[r=0,g=0,b=0], E=java.awt.Color[r=0,g=0,b=0], F=java.awt.Color[r=0,g=0,b=0]}}, B={p={B=java.awt.Color[r=0,g=0,b=0]}}, C={q={C=java.awt.Color[r=0,g=0,b=0]}}, D={r={D=java.awt.Color[r=0,g=0,b=0]}}, E={ADD_u={E=java.awt.Color[r=178,g=178,b=0]}, REM_s={E=java.awt.Color[r=178,g=178,b=0]}}, F={ADD_t={F=java.awt.Color[r=178,g=178,b=0]}, REM_v={F=java.awt.Color[r=178,g=178,b=0]}}}",
				((EdgeAnnotation)graph.getUserDatum(JUConstants.EDGE)).toString());
	}
	
	/** Disconnected states and falling back to initial pair in GD which causes the collection of scores to be unfilled. */
	@SuppressWarnings("unchecked")
	@Test
	public final void testVisual5b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setTransitionMatrixImplType(STATETREE.STATETREE_SLOWTREE);
		config.setGdKeyPairThreshold(1);config.setGdLowToHighRatio(1);config.setGdMaxNumberOfStatesInCrossProduct(0);
		String name = "testVisual5";
		String common = "A-a->B-p->B\nA-a->C-q->C\nA-a->D-r->D\nS-a-#T";
		LearnerGraphND grA = FsmParser.buildLearnerGraphND("A-a->E-s->E\nA-a->F-v->F\nU-a-#V\n"+common,name+"A",config,null);
		LearnerGraphND grB = FsmParser.buildLearnerGraphND("A-a->G-u->G\nA-a->H-t->H\n"+common,name+"B",config,null);
		LearnerGraphND grA_reduced = new LearnerGraphND(config), grB_reduced = new LearnerGraphND(config);
		AbstractPathRoutines.removeRejectStates(grA, grA_reduced);
		AbstractPathRoutines.removeRejectStates(grB, grB_reduced);
		GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData> gd = 
			new GD<List<CmpVertex>,List<CmpVertex>,LearnerGraphNDCachedData,LearnerGraphNDCachedData>();
		DirectedSparseGraph graph = gd.showGD(grA_reduced,grB_reduced,	ExperimentRunner.getCpuNumber());
		Assert.assertEquals("{A=(K 0,0=A), B=(P), C=(P), D=(P), E=(P)[G] , F=(P)[H] , S=KEPT, U=DEL}",((Map<String,String>)graph.getUserDatum(JUConstants.VERTEX)).toString());
		Assert.assertEquals("{A={a={B=java.awt.Color[r=0,g=0,b=0], C=java.awt.Color[r=0,g=0,b=0], D=java.awt.Color[r=0,g=0,b=0], E=java.awt.Color[r=0,g=0,b=0], F=java.awt.Color[r=0,g=0,b=0]}}, B={p={B=java.awt.Color[r=0,g=0,b=0]}}, C={q={C=java.awt.Color[r=0,g=0,b=0]}}, D={r={D=java.awt.Color[r=0,g=0,b=0]}}, E={ADD_u={E=java.awt.Color[r=178,g=178,b=0]}, REM_s={E=java.awt.Color[r=178,g=178,b=0]}}, F={ADD_t={F=java.awt.Color[r=178,g=178,b=0]}, REM_v={F=java.awt.Color[r=178,g=178,b=0]}}}",
				((EdgeAnnotation)graph.getUserDatum(JUConstants.EDGE)).toString());
	}
	
	@Test
	public final void testVisual6()
	{
		Pair<String,String> pair = differenceLabelling("A-a->C-u->C-c->F\nC-v->C\nC-c->G\nC-c->A\nC-b->A\n"+
				"G-b->A\nG-b->C\nG-b->F\n"+
				"F-a->A\nF-a->C\nF-a->G\n","B-a->D-u->D-v->D-b->C-bD->D\nC-bA->A\nC-bG->G\nC-bE->E\n");
		Assert.assertEquals("{A=(P=C,0.28527777777777774)[C] , C=(K 6,1=D)[D] , E=ADD, F=(K 5,2=B)[B] , G=KEPT, P1000=DUP[A] }",pair.firstElem);
		Assert.assertEquals("{A={ADD_bA={P1000=java.awt.Color[r=0,g=255,b=0]}, ADD_bD={C=java.awt.Color[r=178,g=178,b=0]}, ADD_bE={E=java.awt.Color[r=0,g=255,b=0]}, ADD_bG={G=java.awt.Color[r=0,g=255,b=0]}, REM_a={C=java.awt.Color[r=178,g=178,b=0]}}, C={REM_c={A=java.awt.Color[r=124,g=0,b=0], F=java.awt.Color[r=255,g=0,b=0], G=java.awt.Color[r=255,g=0,b=0]}, b={A=java.awt.Color[r=124,g=0,b=0]}, u={C=java.awt.Color[r=0,g=0,b=0]}, v={C=java.awt.Color[r=0,g=0,b=0]}}, F={REM_a={A=java.awt.Color[r=255,g=0,b=0], G=java.awt.Color[r=255,g=0,b=0]}, a={C=java.awt.Color[r=0,g=0,b=0]}}, G={REM_b={A=java.awt.Color[r=255,g=0,b=0], C=java.awt.Color[r=255,g=0,b=0], F=java.awt.Color[r=255,g=0,b=0]}}}",
					pair.secondElem);
	}
	
	@Test
	public final void testVisual7()
	{
		Pair<String,String> pair = differenceLabelling("A2-a->B2 / B2-a->C2","A3-a->D3 / D3-a->C3 / D3-b->D3");
		Assert.assertEquals("{A2=(K 5,2=A3)[A3] , B2=(P=D3,0.5)[D3] , C2=(K 5,2=C3)[C3] }",pair.firstElem);
		Assert.assertEquals("{A2={a={B2=java.awt.Color[r=0,g=0,b=0]}}, B2={ADD_b={B2=java.awt.Color[r=0,g=255,b=0]}, a={C2=java.awt.Color[r=0,g=0,b=0]}}}",
					pair.secondElem);
	}
}
