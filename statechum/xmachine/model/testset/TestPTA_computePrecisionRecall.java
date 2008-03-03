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

import statechum.analysis.learning.Configuration;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.xmachine.model.testset.PTATestSequenceEngine.sequenceSet;

import static org.junit.Assert.assertEquals;

public class TestPTA_computePrecisionRecall {

	private PTATestSequenceEngine en = null; 
	private LearnerGraph fsm = null;
	
	/** The configuration to use in these tests. */
	private Configuration config = Configuration.getDefaultConfiguration();
	
	@Before
	public final void setUp()
	{
		fsm = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->A-b-#C\nB-b->D-c->E\nD-a-#F", "testPrecisionRecall0"),config);
		en = new PTA_FSMStructure(fsm);		
	}
	
	@Test
	public final void testPrecisionRecall0()
	{
		sequenceSet seq = en.new sequenceSet();
		seq.crossWithSet(Arrays.asList(new String[] {"a"})); // appending anything to an empty sequence produces an empty sequence.
		TestPTATestSequenceEngine.vertifyPTA(en, new String[][] {
				new String[] {}
		});
	}
	
	@Test
	public final void testPrecisionRecall1()
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b-#B","testPrecisionRecall1"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, new String[]{"b"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(3, precComputer.pos_relret);assertEquals(1, precComputer.neg_relret);
		assertEquals(3, precComputer.pos_Rel);assertEquals(1, precComputer.neg_Rel);
		assertEquals(3, precComputer.pos_Ret);assertEquals(1, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall2()
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b-#B","testPrecisionRecall2"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, new String[]{"b"}, new String[]{"a", "b"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(3, precComputer.pos_relret);assertEquals(1, precComputer.neg_relret);
		assertEquals(3, precComputer.pos_Rel);assertEquals(2, precComputer.neg_Rel);
		assertEquals(4, precComputer.pos_Ret);assertEquals(1, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall3()
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b-#B","testPrecisionRecall3"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, new String[]{"b"}, new String[]{"a", "b"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(4, precComputer.pos_relret);assertEquals(2, precComputer.neg_relret);
		assertEquals(4, precComputer.pos_Rel);assertEquals(3, precComputer.neg_Rel);
		assertEquals(5, precComputer.pos_Ret);assertEquals(2, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall4()
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->BM-a->AM\nBM-b->CM-a->DM","testPrecisionRecall4"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, new String[]{"b"}, new String[]{"a", "b", "c"}, new String[]{"a", "b", "a"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(5, precComputer.pos_relret);assertEquals(2, precComputer.neg_relret);
		assertEquals(6, precComputer.pos_Rel);assertEquals(3, precComputer.neg_Rel);
		assertEquals(6, precComputer.pos_Ret);assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall5() // long test sequence (which exists) which is rejected part-way 
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall5"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, 
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(2, precComputer.pos_relret);assertEquals(0, precComputer.neg_relret);
		assertEquals(5, precComputer.pos_Rel);assertEquals(0, precComputer.neg_Rel);
		assertEquals(2, precComputer.pos_Ret);assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall6() // long test sequence (which does not exist) which is rejected part-way 
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall6"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c","d"}, 
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals(2, precComputer.pos_relret);assertEquals(1, precComputer.neg_relret);
		assertEquals(5, precComputer.pos_Rel);assertEquals(1, precComputer.neg_Rel);
		assertEquals(2, precComputer.pos_Ret);assertEquals(4, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall_ign1() // long test sequence (which exists) which is rejected part-way, but first element of it is ignored. 
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);

		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a"}, 
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach);
		partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, 
			}));
		precComputer.crossWith(engine);
		assertEquals(1, precComputer.pos_relret);assertEquals(0, precComputer.neg_relret);
		assertEquals(4, precComputer.pos_Rel);assertEquals(0, precComputer.neg_Rel);
		assertEquals(1, precComputer.pos_Ret);assertEquals(3, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall_ign2() // a complex structure, most of which gets ignored.
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->BM-a->AM\nBM-b->CM-a->DM","testPrecisionRecall_ign2"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, new String[]{"b"}, new String[]{"a", "b", "c"}, new String[]{"a", "b", "a"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		precComputer.crossWith(engine);
		assertEquals(0, precComputer.pos_relret);assertEquals(0, precComputer.neg_relret);
		assertEquals(0, precComputer.pos_Rel);assertEquals(0, precComputer.neg_Rel);
		assertEquals(0, precComputer.pos_Ret);assertEquals(0, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall_ign3() // long test sequence (which does not exist) which is rejected part-way 
	{
		LearnerGraph mach = new LearnerGraph(TestFSMAlgo.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign3"),config);
		PTATestSequenceEngine engine = new PTA_FSMStructure(mach);
		sequenceSet partialPTA = engine.new sequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c","d"}, 
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		precComputer.crossWith(engine);
		assertEquals(0, precComputer.pos_relret);assertEquals(1, precComputer.neg_relret);
		assertEquals(2, precComputer.pos_Rel);assertEquals(1, precComputer.neg_Rel);
		assertEquals(0, precComputer.pos_Ret);assertEquals(3, precComputer.neg_Ret);
	}
}
