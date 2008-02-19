/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.xmachine.model.testset;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;

import static org.junit.Assert.assertEquals;

public class TestPTA_computePrecisionRecall {

	private PTATestSequenceEngine en = null;

	private FSMStructure fsm = null;

	@Before
	public final void setUp() {
		fsm = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->B-a->A-b-#C\nB-b->D-c->E\nD-a-#F", "test automaton"));
		en = new PTA_FSMStructure(fsm);
	}

	@Test
	public final void test_sequenceSet1() {
		sequenceSet seq = en.new sequenceSet();
		seq.crossWithSet(Arrays.asList(new String[] { "a" })); // appending
																// anything to
																// an empty
																// sequence
																// produces an
																// empty
																// sequence.
		TestPTATestSequenceEngine.vertifyPTA(en,
				new String[][] { new String[] {} });
	}

	@Test
	public final void test_1() {
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->A-b-#B", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] { "a", "a", "a" }, new String[] { "b" } }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(3, precComputer.pos_relret);
		assertEquals(1, precComputer.neg_relret);
		assertEquals(3, precComputer.pos_Rel);
		assertEquals(1, precComputer.neg_Rel);
		assertEquals(3, precComputer.pos_Ret);
		assertEquals(1, precComputer.neg_Ret);
	}

	@Test
	public final void test_2() {
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->A-b-#B", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] { "a", "a", "a" }, new String[] { "b" },
				new String[] { "a", "b" } }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(3, precComputer.pos_relret);
		assertEquals(1, precComputer.neg_relret);
		assertEquals(3, precComputer.pos_Rel);
		assertEquals(2, precComputer.neg_Rel);
		assertEquals(4, precComputer.pos_Ret);
		assertEquals(1, precComputer.neg_Ret);
	}

	@Test
	public final void test_3() {
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"A-a->A-b-#B", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] { "a", "a", "a", "a", "b" }, new String[] { "b" },
				new String[] { "a", "b" } }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(4, precComputer.pos_relret);
		assertEquals(2, precComputer.neg_relret);
		assertEquals(4, precComputer.pos_Rel);
		assertEquals(3, precComputer.neg_Rel);
		assertEquals(5, precComputer.pos_Ret);
		assertEquals(2, precComputer.neg_Ret);
	}

	@Test
	public final void test_4() {
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->BM-a->AM\nBM-b->CM-a->DM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] {
						new String[] { "a", "a", "a", "a", "b" },
						new String[] { "b" }, new String[] { "a", "b", "c" },
						new String[] { "a", "b", "a" } }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(5, precComputer.pos_relret);
		assertEquals(2, precComputer.neg_relret);
		assertEquals(6, precComputer.pos_Rel);
		assertEquals(3, precComputer.neg_Rel);
		assertEquals(6, precComputer.pos_Ret);
		assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void test_5() // long test sequence (which exists) which is
								// rejected part-way
	{
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->AM-b->AM-c->AM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] { new String[] { "a", "b", "b", "c",
						"c" }, }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(2, precComputer.pos_relret);
		assertEquals(0, precComputer.neg_relret);
		assertEquals(5, precComputer.pos_Rel);
		assertEquals(0, precComputer.neg_Rel);
		assertEquals(2, precComputer.pos_Ret);
		assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void test_6() // long test sequence (which does not exist)
								// which is rejected part-way
	{
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->AM-b->AM-c->AM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] { new String[] { "a", "b", "b", "c",
						"c", "d" }, }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		assertEquals(2, precComputer.pos_relret);
		assertEquals(1, precComputer.neg_relret);
		assertEquals(5, precComputer.pos_Rel);
		assertEquals(1, precComputer.neg_Rel);
		assertEquals(2, precComputer.pos_Ret);
		assertEquals(4, precComputer.neg_Ret);
	}

	@Test
	public final void test_ign_1() // long test sequence (which exists) which
									// is rejected part-way, but first element
									// of it is ignored.
	{
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->AM-b->AM-c->AM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);

		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] { new String[] { "a" }, }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);

		engine = new PTA_FSMStructure(mach);
		partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] { new String[] { "a", "b", "b", "c",
						"c" }, }));
		precComputer.crossWith(engine);
		assertEquals(1, precComputer.pos_relret);
		assertEquals(0, precComputer.neg_relret);
		assertEquals(4, precComputer.pos_Rel);
		assertEquals(0, precComputer.neg_Rel);
		assertEquals(1, precComputer.pos_Ret);
		assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void test_ign2() // a complex structure, most of which gets
									// ignored.
	{
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->BM-a->AM\nBM-b->CM-a->DM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] {
						new String[] { "a", "a", "a", "a", "b" },
						new String[] { "b" }, new String[] { "a", "b", "c" },
						new String[] { "a", "b", "a" } }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		precComputer.crossWith(engine);
		assertEquals(0, precComputer.pos_relret);
		assertEquals(0, precComputer.neg_relret);
		assertEquals(0, precComputer.pos_Rel);
		assertEquals(0, precComputer.neg_Rel);
		assertEquals(0, precComputer.pos_Ret);
		assertEquals(0, precComputer.neg_Ret);
	}

	@Test
	public final void test_ign3() // long test sequence (which does not exist)
									// which is rejected part-way
	{
		FSMStructure mach = WMethod.getGraphData(TestFSMAlgo.buildGraph(
				"AM-a->AM-b->AM-c->AM", "tmp graph"));
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();
		partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo
				.buildSet(new String[][] { new String[] { "a", "b", "b", "c",
						"c", "d" }, }));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(
				fsm);
		precComputer.crossWith(engine);
		precComputer.crossWith(engine);
		assertEquals(0, precComputer.pos_relret);
		assertEquals(1, precComputer.neg_relret);
		assertEquals(2, precComputer.pos_Rel);
		assertEquals(1, precComputer.neg_Rel);
		assertEquals(0, precComputer.pos_Ret);
		assertEquals(3, precComputer.neg_Ret);
	}
}
