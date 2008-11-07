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

package statechum.analysis.learning.rpnicore;

import java.util.Arrays;
import java.util.HashSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;

public class TestTransitionMatrixND {
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();		
		ba=new LTL_to_ba(config);ba.alphabet = new HashSet<String>();
		ba.alphabet.addAll(Arrays.asList(new String[]{"a","b","c"}));
		
		expectedFromASEExample = new LearnerGraph(TestFSMAlgo.buildGraph(
				"I-close->1\nI-edit->I1\nI-save->I1\nI-load->I1\n"+
				"1-load->I1-close->1\n"+
				"I1-edit->I1-save->I1-load->I1\n","testLTL_bigger"),config);
	}
	// ,"load","save","edit","close"
	protected Configuration config = null;
	protected LTL_to_ba ba = null;
	protected LearnerGraph expectedFromASEExample = null;
	


	@Test
	public final void testLTL_complete2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-c->B-b->B", "testLTL_ba_graph3"),config);
		LearnerGraph result = ba.completeMatrix(graph);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B\n"+
				"A-b-#R1\n"+"B-a-#R2\n"+"B-c-#R3"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add1()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(TransitionMatrixND.convertToND(graph),graphToAdd).buildDeterministicGraph(config);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add2()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(TransitionMatrixND.convertToND(graph),graphToAdd).buildDeterministicGraph(config);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add3()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-b->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(TransitionMatrixND.convertToND(graph),graphToAdd).buildDeterministicGraph(config);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"+
				"A-b->S"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}
	
	/** Tests of adding to graph. */
	@Test
	public final void testLTL_add4()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("init-a->init-d->S", "testLTL_ba_graph3"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-c->B-b->B-a->A-d->E-d->F", "testLTL_add1"),config);
		
		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(TransitionMatrixND.convertToND(graph),graphToAdd).buildDeterministicGraph(config);
		LearnerGraph expected = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-c->B-b->B-a->A-d->E-d->F\n"
				, "testLTL_complete2"),config);
		Assert.assertNull(WMethod.checkM(result,expected));
	}

	@Test
	public final void testLTL_add5()
	{
		LearnerGraph graph = new LearnerGraph(TestFSMAlgo.buildGraph("P-a->Q-a->S-a->U\nP-b->R-a->T", "testLTL_add5_A"),config);
		LearnerGraph graphToAdd = new LearnerGraph(TestFSMAlgo.buildGraph("A-b->A-a->B-a->B-b-#C", "testLTL_add5_B"),config);

		LearnerGraph result = TransitionMatrixND.UniteTransitionMatrices(TransitionMatrixND.convertToND(graph),graphToAdd).buildDeterministicGraph(config);
		Assert.assertNull(WMethod.checkM(graphToAdd,result));
	}
	
}
