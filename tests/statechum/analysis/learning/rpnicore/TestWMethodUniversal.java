/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

package statechum.analysis.learning.rpnicore;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Helper;
import statechum.Label;
import statechum.Configuration.STATETREE;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper.whatToRun;
import statechum.JUConstants.PAIRCOMPATIBILITY;
import statechum.Pair;
import statechum.analysis.learning.Visualiser;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.FsmPermutator;

@RunWith(ParameterizedWithName.class)
public class TestWMethodUniversal 
{
	boolean prefixClosed;
	
	public TestWMethodUniversal(Configuration conf, boolean closed)
	{
		prefixClosed = closed;
		config = conf.copy();
		converter = config.getTransitionMatrixImplType() == STATETREE.STATETREE_ARRAY?new Transform.InternStringLabel():null;
	}
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		List<Object[]> outcome = new LinkedList<Object[]>();
		for(Object[] cnf:TestWithMultipleConfigurations.data())
		{
			assert cnf.length == 1;
			outcome.add(new Object[]{cnf[0],true});outcome.add(new Object[]{cnf[0],false});
		}
		return outcome;
	}

	/** Given an argument, returns a textual representation of it. 
	 * 
	 * @param config configuration to use
	 * @param closed boolean to consider
	 * @return description.
	 */ 
	@org.junit.runners.ParameterizedWithName.ParametersToString
	public static String parametersToString(Configuration config,Boolean closed)
	{
		return TestWithMultipleConfigurations.parametersToString(config)+" "+(closed?"prefix-closed":"not prefix-closed");
	}
	
	/** The configuration to use when running tests. */
	private final Configuration config;
	private final ConvertALabel converter;
	
	public void testWsetconstruction(String machine, boolean equivalentExpected, boolean reductionExpected)
	{
		LearnerGraph fsm = buildLearnerGraph(machine,"testWset",config,converter);
		statechum.analysis.learning.rpnicore.TestWMethod.testWsetconstruction(fsm,equivalentExpected,reductionExpected,prefixClosed);
	}
	

	@Test
	public final void testWset1()
	{
		testWsetconstruction("A-p->A-b->B-c->B-a->C",false,true);
	}
	
	@Test
	public final void testWset2()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-a->Q",false,true);
	}
	
	/** Equivalent states. */
	@Test
	public final void testWset3()
	{
		testWsetconstruction("A-a->C-b->Q\nB-a->D-b->Q",true,true);
	}

	@Test
	public final void testWset4()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->A","testWset4",config,converter);
		Assert.assertTrue(WMethod.computeWSet_reducedmemory(fsm).isEmpty());
	}

	/** Equivalent states. */
	@Test
	public final void testWset5a()
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true,true);
	}
	
	/** Equivalent states. */
	@Test
	public final void testWset5b()
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-a->K1\nB-a->B1-z->B2-b->K1\nC-a->C1-b->C2-a->K2-b->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				true,true);
	}
	
	/** Equivalent states. */
	@Test
	public final void testWset6()
	{
		testWsetconstruction("S-a->A\nS-b->B\nS-c->C\nS-d->D\nS-e->E\nS-f->F\nS-h->H-d->H\nA-a->A1-b->A2-a->K1-m->K1\nB-a->B1-b->B2-b->K1\nC-a->C1-b->C2-a->K2-z->K2\nD-a->D1-b->D2-b->K2\nE-a->E1-b->E2-a->K3-c->K3\nF-a->F1-b->F2-b->K3",
				false,true);
	}

	@Test
	public final void testWset7()
	{
		testWsetconstruction("A-a->B-a->C-a->A-b->C-b->B",false,true);
	}
	
	@Test
	public final void testWset8()
	{
		testWsetconstruction("S-a->A-a->D-a->D-b->A-b->B-a->D\nB-b->C-a->D\nC-b->D\nS-b->N-a->M-a->N\nN-b->M-b->N",true,true);
	}
	
	@Test
	public final void testWset9()
	{
		testWsetconstruction("A-a->D\nB-a->C\nA-b->B\nD-b->C",false,true);
	}
	
	@Test
	public final void testWset10()
	{
		String machineOrig = "0--a4->0--a2->0--a5->2\n0--a7->4\n0--a9->3\n0--a0->1\n1--a5->0\n1--a3->0\n4--a1->0\n4--a8->3\n3--a4->1\n3--a6->2\n2--a8->4\n2--a4->0\n3--a9->0",
			machine = null;
		int a = 4, b = 2;
		machine = machineOrig.replaceAll(a+"--", "Q"+"--").replaceAll(">"+a, ">"+"Q")
			.replaceAll(b+"--", a+"--").replaceAll(">"+b, ">"+a)
			.replaceAll("Q"+"--", b+"--").replaceAll(">"+"Q", ">"+b);
		testWsetconstruction(machine,false,true);
	}

	@Test
	public final void testWset11()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n",false,true);
	}

	@Test
	public final void testWset12()
	{
		testWsetconstruction("0-a0->1\n0-a1->8\n0-a2->7\n0-a7->3\n0-a9->3\n0-a11->8\n0-a12->6\n0-a15->0\n1-a2->0\n1-a4->6\n1-a6->4\n1-a12->5\n1-a13->5\n1-a16->8\n1-a18->6\n1-a19->2\n2-a2->2\n2-a5->7\n2-a8->0\n2-a10->8\n2-a12->8\n2-a13->1\n2-a14->5\n2-a16->8\n3-a3->3\n3-a6->2\n3-a8->7\n3-a10->4\n3-a11->6\n3-a14->9\n3-a15->3\n3-a16->7\n4-a0->4\n4-a3->1\n4-a5->6\n4-a6->7\n4-a10->7\n4-a12->3\n4-a17->4\n4-a18->4\n5-a0->0\n5-a6->3\n5-a7->0\n5-a11->0\n5-a14->4\n5-a16->3\n5-a17->3\n5-a18->4\n6-a0->6\n6-a2->2\n6-a4->1\n6-a10->9\n6-a11->2\n6-a12->1\n6-a17->5\n6-a19->9\n7-a1->5\n7-a2->9\n7-a3->5\n7-a5->1\n7-a7->2\n7-a10->1\n7-a11->0\n7-a16->9\n8-a3->9\n8-a4->9\n8-a5->6\n8-a6->8\n8-a7->6\n8-a12->8\n8-a17->5\n8-a18->9\n9-a1->7\n9-a5->5\n9-a9->1\n9-a10->7\n9-a15->2\n9-a17->0\n9-a18->2\n9-a19->4\n",false,true);
	}

	@Test
	public final void testWset13()
	{
		testWsetconstruction("0-a0->1\n0-a1->9\n0-a2->5\n0-a4->9\n0-a5->5\n0-a10->9\n0-a11->7\n0-a12->9\n0-a15->8\n0-a16->0\n0-a17->3\n0-a18->8\n1-a0->7\n1-a2->0\n1-a3->2\n1-a4->7\n1-a5->6\n1-a7->6\n1-a8->2\n1-a11->0\n1-a15->0\n1-a17->4\n1-a18->1\n1-a19->4\n2-a0->5\n2-a4->7\n2-a5->0\n2-a6->1\n2-a8->9\n2-a10->9\n2-a11->6\n2-a12->2\n2-a13->6\n2-a15->3\n2-a16->1\n2-a17->0\n3-a1->8\n3-a3->3\n3-a4->5\n3-a6->4\n3-a7->6\n3-a9->1\n3-a10->4\n3-a11->3\n3-a15->6\n3-a16->6\n3-a17->5\n3-a19->6\n4-a0->4\n4-a1->0\n4-a2->5\n4-a3->3\n4-a6->2\n4-a7->2\n4-a8->8\n4-a9->0\n4-a10->5\n4-a16->2\n4-a17->5\n4-a19->4\n5-a0->9\n5-a2->6\n5-a5->1\n5-a7->5\n5-a8->4\n5-a9->2\n5-a11->4\n5-a12->7\n5-a15->7\n5-a17->1\n5-a18->7\n5-a19->7\n6-a4->4\n6-a6->7\n6-a7->9\n6-a9->2\n6-a10->8\n6-a12->3\n6-a13->7\n6-a14->1\n6-a15->8\n6-a16->0\n6-a17->1\n6-a18->2\n7-a1->6\n7-a3->1\n7-a5->2\n7-a6->0\n7-a7->6\n7-a8->3\n7-a9->0\n7-a10->5\n7-a11->4\n7-a15->8\n7-a17->8\n7-a19->2\n8-a0->7\n8-a1->3\n8-a3->9\n8-a4->7\n8-a5->6\n8-a6->1\n8-a8->3\n8-a9->0\n8-a10->3\n8-a11->9\n8-a14->8\n8-a18->4\n9-a0->4\n9-a1->9\n9-a5->8\n9-a6->5\n9-a7->3\n9-a9->9\n9-a10->3\n9-a13->8\n9-a14->5\n9-a15->1\n9-a16->8\n9-a17->2\n",false,false);
	}

	
	/** Multiple equivalent states. */
	@Test
	public final void testWset14()
	{
		testWsetconstruction("A-a->B-a->B2-a->B-b->C-b->C-c->D / B2-b->C2-b->C3-b->C4-b->C2-c->D2 / C3-c->D2 / C4-c->D2",true,false);
	}
	
	/** A big graph */
	@Test
	public final void testWsetBig1() throws IOException
	{
		LearnerGraph fsm = new LearnerGraph(config.copy());
		AbstractPersistence.loadGraph(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.RESOURCES)+File.separator+"testWset1.graphml",fsm,converter);
		statechum.analysis.learning.rpnicore.TestWMethod.testWsetconstruction(fsm,true,false,prefixClosed);
	}
	
	/** Multiple non-equivalent states in the presence of incompatible states - should be ignored. */
	@Test
	public final void testWset_incompatibles1()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B-a->B-b->C-b->C-c->D","testWset_incompatibles1",config,converter);
		fsm.addToCompatibility(fsm.findVertex("A"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		statechum.analysis.learning.rpnicore.TestWMethod.testWsetconstruction(fsm,false,false,prefixClosed);
	}
	
	/** Multiple non-equivalent states in different equivalence classes - should be ignored. */
	@Test
	public final void testWset_incompatibles2()
	{
		LearnerGraph fsm = buildLearnerGraph("A-a->B-a->B2-a->B-b->C-b->C-c->D-d->A / B2-b->C2-b->C3-b->C4-b->C2-c->D2 / C3-c->D2 / C4-c->D2-d->A","testWset_incompatibles2",config,converter);
		fsm.addToCompatibility(fsm.findVertex("C"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C"), fsm.findVertex("B2"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C2"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C2"), fsm.findVertex("B2"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C3"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C3"), fsm.findVertex("B2"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C4"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		fsm.addToCompatibility(fsm.findVertex("C4"), fsm.findVertex("B2"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		statechum.analysis.learning.rpnicore.TestWMethod.testWsetconstruction(fsm,true,false,prefixClosed);
	}
	
	/** Multiple non-equivalent states in the same equivalence classes - exception has to be thrown. */
	@Test
	public final void testWset_incompatibles_fail1()
	{
		final LearnerGraph fsm = buildLearnerGraph("A-a->B-a->B2-a->B-b->C-b->C-c->D / B2-b->C2-b->C3-b->C4-b->C2-c->D2 / C3-c->D2 / C4-c->D2","testWset_incompatibles2",config,converter);
		fsm.addToCompatibility(fsm.findVertex("B2"), fsm.findVertex("B"), PAIRCOMPATIBILITY.INCOMPATIBLE);
		Helper.checkForCorrectException(new whatToRun() { public @Override void run() {
			statechum.analysis.learning.rpnicore.TestWMethod.testWsetconstruction(fsm,true,false,prefixClosed);
		}},IllegalArgumentException.class,"equivalent states cannot be incompatible");
	}
	
	public class EmptyPermutator implements FsmPermutator {
		@Override 
		public ArrayList<Pair<CmpVertex,Label>> getPermutation(Collection<Pair<CmpVertex,Label>> from) 
		{
			ArrayList<Pair<CmpVertex,Label>> result = new ArrayList<Pair<CmpVertex,Label>>(from.size());
			result.addAll(from);
			return result;
		}
	}

	public class RandomPermutator implements FsmPermutator {
		private Random rnd = null;
		
		public RandomPermutator(int randomArg)
		{
			rnd = new Random(randomArg);
		}
		/** Returns an array representing an order in which elements of an FSM should be placed in a string. */
		@Override 
		public ArrayList<Pair<CmpVertex,Label>> getPermutation(Collection<Pair<CmpVertex,Label>> from) 
		{
			ArrayList<Pair<CmpVertex,Label>> result = new ArrayList<Pair<CmpVertex,Label>>(from.size());
			result.addAll(from);
			
			for(int i=0;i< from.size();++i)
			{
				int first = rnd.nextInt(from.size()), second = rnd.nextInt(from.size());
				Pair<CmpVertex,Label> firstObj = result.get(first);result.set(first,result.get(second));result.set(second,firstObj);
			}
			return result;
		}
	}
	
	/** Given a machine, this method permutes its states. Tested as a part of testing that 
	 * W set generation is not affected by the order in which states are presented in
	 * a string from which a machine is built.
	 * 
	 * @param machine machine to permute
	 * @param perm the permutator function
	 * @param testName the name to give to the generated machine
	 */
	public void testWsetDeterministic(String machine, FsmPermutator perm, String testName)
	{
		LearnerGraph fsm = buildLearnerGraph(machine,"testDeterminism_"+testName,config,converter);
		Set<List<Label>> origWset = new HashSet<List<Label>>();origWset.addAll(WMethod.computeWSet_reducedmemory(fsm));
		LearnerGraph permFsm = fsm.wmethod.Permute(perm,converter);
		Assert.assertNull(WMethod.checkM(fsm,permFsm));
		
		Set<List<Label>> newWset = new HashSet<List<Label>>();newWset.addAll(WMethod.computeWSet_reducedmemory(permFsm));
		fsm.wmethod.checkW_is_corrent(newWset,prefixClosed,null);
		fsm.wmethod.checkW_is_corrent(origWset,prefixClosed,null);
		permFsm.wmethod.checkW_is_corrent(newWset,prefixClosed,null);
		permFsm.wmethod.checkW_is_corrent(origWset,prefixClosed,null);
		
		Assert.assertTrue(origWset.equals(newWset));
	}

	@Test
	public final void testDeterminism()
	{
		String machine = "0-a0->1\n0-a1->9\n0-a2->6\n0-a3->1\n0-a5->0\n0-a7->7\n0-a10->7\n0-a12->5\n1-a0->9\n1-a1->5\n1-a3->3\n1-a6->3\n1-a8->7\n1-a14->9\n1-a17->9\n1-a18->6\n2-a0->8\n2-a2->8\n2-a3->6\n2-a4->4\n2-a7->3\n2-a9->2\n2-a10->4\n2-a15->5\n3-a0->5\n3-a1->2\n3-a2->2\n3-a7->3\n3-a9->8\n3-a10->0\n3-a15->6\n3-a16->5\n4-a0->8\n4-a4->8\n4-a5->0\n4-a7->4\n4-a11->0\n4-a12->3\n4-a16->0\n4-a19->5\n5-a0->1\n5-a2->1\n5-a5->6\n5-a6->2\n5-a7->9\n5-a9->0\n5-a11->3\n5-a19->5\n6-a0->1\n6-a2->4\n6-a4->7\n6-a9->8\n6-a10->0\n6-a12->1\n6-a18->1\n6-a19->3\n7-a1->6\n7-a5->4\n7-a7->9\n7-a10->9\n7-a12->7\n7-a13->4\n7-a14->6\n7-a15->9\n8-a2->7\n8-a4->1\n8-a5->6\n8-a6->4\n8-a9->0\n8-a11->2\n8-a13->2\n8-a14->7\n9-a2->7\n9-a3->3\n9-a5->4\n9-a6->2\n9-a9->5\n9-a11->2\n9-a16->8\n9-a17->8\n";
		testWsetDeterministic(machine, new EmptyPermutator(), "testDeterminism1_empty");
		for(int i=0;i<100;++i)
			testWsetDeterministic(machine, new RandomPermutator(i), "testDeterminism1_random");
	}
	
	@BeforeClass
	public static void initJungViewer() // initialisation - once only for all tests in this class
	{
		Visualiser.disposeFrame();
	}

	@AfterClass
	public static void cleanUp()
	{
		Visualiser.disposeFrame();
	}
	
	/** In order to be able to use old junit runner.
	public static junit.framework.Test suite()
	{
		return new JUnit4TestAdapter(TestWMethod.class);
	}
	*/
}
