/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
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

package statechum.model.testset;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.DeterministicDirectedSparseGraph.VertexID;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.PrecisionRecall.PosNegPrecisionRecall;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.model.testset.PTASequenceEngine.FilterPredicate;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

import static org.junit.Assert.assertEquals;

public class TestPTA_computePrecisionRecall {

	/** The configuration to use in these tests. */
	private Configuration config = Configuration.getDefaultConfiguration();
	LearnerGraph fsm = null;
	@Before
	public final void setUp()
	{
		fsm = new LearnerGraph(FsmParser.buildGraph("A-a->B-a->A-b-#C\nB-b->D-c->E\nD-a-#F", "testPrecisionRecall0"),config);
	}
	
	@Test
	public final void testPrecisionRecall0()
	{
		PTASequenceEngine en = new PTA_FSMStructure(fsm,null);
		SequenceSet seq = en.new SequenceSet();
		seq.crossWithSet(Arrays.asList(new String[] {"a"})); // appending anything to an empty sequence produces an empty sequence.
		TestPTASequenceEngine.vertifyPTA(en, 1, new String[][] {
				new String[] {}
		});
	}
	
	@Test
	public final void testPrecisionRecall1()
	{
		// this graph gives the labelling to the sequences I add to the set of sequences with partialPTA.cross below.
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("A-a->A-b-#B","testPrecisionRecall1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, // + 
				new String[]{"b"}			// -
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",1, precComputer.resultTN);assertEquals("true positives",1, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",1, precComputer.neg_Rel);
		assertEquals("positives retrieved",1, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall2a()
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("A-a->A-b-#B","testPrecisionRecall2a"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"},// + 
				new String[]{"b"},      // -
				new String[]{"a", "b"}  // -, FP
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",1, precComputer.resultTN);assertEquals("true positives",1, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",1, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",2, precComputer.neg_Rel);
		assertEquals("positives retrieved",2, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** Same as 2a but using a different state as an initial state. */
	@Test
	public final void testPrecisionRecall2b()
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("A-a->A-b-#B","testPrecisionRecall2b"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, // + 
				new String[]{"b"}, // -
				new String[]{"a", "b"} // -, FP
			}));
		fsm = new LearnerGraph(FsmParser.buildGraph("Q-a->Q / A-a->B-a->A-b-#C\nB-b->D-c->E\nD-a-#F", "testPrecisionRecall2b"),config);
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm,fsm.findVertex(VertexID.parseID("A")));
		precComputer.crossWith(engine);
		assertEquals("true negatives",1, precComputer.resultTN);assertEquals("true positives",1, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",1, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",2, precComputer.neg_Rel);
		assertEquals("positives retrieved",2, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}
	
	@Test
	public final void testPrecisionRecall3()
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("A-a->A-b-#B","testPrecisionRecall3"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, // - 
				new String[]{"b"}, // -
				new String[]{"a", "b"} //, FP
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",2, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",1, precComputer.resultFP);
		assertEquals("positives relevant",0, precComputer.pos_Rel);assertEquals("negatives relevant",3, precComputer.neg_Rel);
		assertEquals("positives retrieved",1, precComputer.pos_Ret);assertEquals("negatives retrieved",2, precComputer.neg_Ret);
	}

	@Test
	public final void testPrecisionRecall4()
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->BM-a->AM\nBM-b->CM-a->DM","testPrecisionRecall4"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, // -
				new String[]{"b"}, // -
				new String[]{"a", "b", "c"}, // -, FP 
				new String[]{"a", "b", "a"} // +, FN
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",2, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",1, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",3, precComputer.neg_Rel);
		assertEquals("positives retrieved",1, precComputer.pos_Ret);assertEquals("negatives retrieved",3, precComputer.neg_Ret);
	}

	/** long test sequence (which exists) which is rejected part-way. */
	@Test
	public final void testPrecisionRecall5a()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall5"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b","a"}, // +, FN
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** long test sequence which is rejected at the last node. */
	@Test
	public final void testPrecisionRecall5b()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall5"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, // +, FN
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** long test sequence (which does not exist) which is rejected part-way. */
	@Test
	public final void testPrecisionRecall6()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall6"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b","a","c"}, // +, FN
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** a tree of long test sequences (which should not exist) are is rejected part-way. */
	@Test
	public final void testPrecisionRecall7()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall6"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b","a","d"}, // -
				new String[] {"a","a","a","b","a","e","f"}, // -
				new String[] {"a","a","a","b","a","c","c","c"}, // +, FN
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",2, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",2, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",3, precComputer.neg_Ret);
	}

	/** Long test sequences which are accepted. */
	@Test
	public final void testPrecisionRecall8()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall6"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","a","b","c"}, // +
				new String[] {"a","b"}, // +
				new String[] {"a","a","a","b","c"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",3, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",3, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",3, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}

	/** Long test sequence (which exists) which is rejected part-way, but the first element of it does not count. */
	@Test
	public final void testPrecisionRecall_ign1()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, // +, FN
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** Test sequence (which exists) which is rejected part-way, all of it is ignored. */
	@Test
	public final void testPrecisionRecall_ign2()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, // +, FN
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",0, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}

	/** Test sequence (which exists) which is rejected part-way, shorter than a test sequence but is all ignored. */
	@Test
	public final void testPrecisionRecall_ign3()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c","c","c"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);

		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, // +, FN
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** Test sequence (which exists) which is rejected part-way, but first part of it is ignored. */
	@Test
	public final void testPrecisionRecall_ign4()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","b","b","c","c"}, // +, FN
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",1, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",1, precComputer.neg_Ret);
	}

	/** Test sequence (which exists) which is accepted. */
	@Test
	public final void testPrecisionRecall_ign5()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b"}, // +
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",1, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",1, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}
	
	/** Test sequence (which exists) which is accepted. */
	@Test
	public final void testPrecisionRecall_ign6()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a"}, // +
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",1, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",1, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",1, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}
	
	/** Test sequence (which exists), all of it is ignored. */
	@Test
	public final void testPrecisionRecall_ign7()  
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->AM-b->AM-c->AM","testPrecisionRecall_ign1"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b"}, // +
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		
		engine = new PTA_FSMStructure(mach,null);
		partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","b"}, // +
			}));
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",0, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}
	
	@Test
	public final void testPrecisionRecall_ign8() // a complex structure, most of which gets ignored.
	{
		LearnerGraph mach = new LearnerGraph(FsmParser.buildGraph("AM-a->BM-a->AM\nBM-b->CM-a->DM","testPrecisionRecall_ign2"),config);
		PTASequenceEngine engine = new PTA_FSMStructure(mach,null);
		SequenceSet partialPTA = engine.new SequenceSet();partialPTA.setIdentity();
		partialPTA = partialPTA.cross(TestFSMAlgo.buildSet(new String[][] {
				new String[] {"a","a","a","a","b"}, new String[]{"b"}, new String[]{"a", "b", "c"}, new String[]{"a", "b", "a"}
			}));
		PTA_computePrecisionRecall precComputer = new PTA_computePrecisionRecall(fsm);
		precComputer.crossWith(engine);
		precComputer.crossWith(engine);
		assertEquals("true negatives",0, precComputer.resultTN);assertEquals("true positives",0, precComputer.resultTP);
		assertEquals("false negatives",0, precComputer.resultFN);assertEquals("false positives",0, precComputer.resultFP);
		assertEquals("positives relevant",0, precComputer.pos_Rel);assertEquals("negatives relevant",0, precComputer.neg_Rel);
		assertEquals("positives retrieved",0, precComputer.pos_Ret);assertEquals("negatives retrieved",0, precComputer.neg_Ret);
	}

	/** Two-thread computation of precision/recall by going through all the sequences. */
	private static final PosNegPrecisionRecall computePrecisionRecall(final LearnerGraph graph, final PTASequenceEngine sequences)
	{
		final Collection<List<String>> positiveRel = sequences.filter(sequences.getFSM_filterPredicate()).getData(),
			negativeRel = sequences.getData(new FilterPredicate() {
				FilterPredicate origFilter = sequences.getFSM_filterPredicate();
				
				@Override 
				public boolean shouldBeReturned(Object name) {
					return !origFilter.shouldBeReturned(name);
				}
			});
		if (positiveRel.size() == 1 && positiveRel.iterator().next().size() == 0) positiveRel.clear();
		if (negativeRel.size() == 1 && negativeRel.iterator().next().size() == 0) negativeRel.clear();

		final List<List<String>> positiveRet = new LinkedList<List<String>>(), negativeRet = new LinkedList<List<String>>();
		for(List<String> pos:positiveRel)
			if (graph.paths.tracePath(pos, true) == AbstractOracle.USER_ACCEPTED)
				positiveRet.add(pos);
			else
				negativeRet.add(pos);
		
		for(List<String> neg:negativeRel)
			if (graph.paths.tracePath(neg, true) == AbstractOracle.USER_ACCEPTED)
				positiveRet.add(neg);
			else
				negativeRet.add(neg);

		return new PosNegPrecisionRecall(positiveRet, positiveRel, negativeRet, negativeRel);
	}
	
	private class PTA_FSMStructureAccept extends PTASequenceEngine
	{
		public PTA_FSMStructureAccept(final LearnerGraph machine) 
		{
			init(machine.new FSMImplementation() {
				@Override
				public Object getInitState() {
					return machine.getInit();
				} 
				@Override
				public boolean shouldBeReturned(Object elem) 
				{
					if (elem == null) return false;
					return ((CmpVertex)elem).isAccept();
				}
			});
		}
	}
	
	@Test
	public final void testCheckPrecisionRecallBruteForce()
	{
		LearnerGraph graph = new LearnerGraph(FsmParser.buildGraph("A-a->B-c->A / B-b->C-b->B-a-#X / B-c->A / C-a->C","testCheckPrecisionRecallBruteForce_subject"),config);

		// First, sequences are generated
		
		PTASequenceEngine sequences = new PTA_FSMStructureAccept(new LearnerGraph(FsmParser.buildGraph("A-a->A-b->B-b->B-a-#X / B-c->C-c->C-a-#X / C-b->D-a->D-c->D","testCheckPrecisionRecallBruteForce_reference"),config));
		PTASequenceEngine.SequenceSet initSet = sequences.new SequenceSet();initSet.setIdentity();
		Set<String> alphabet = graph.pathroutines.computeAlphabet();
		for(int i=0;i < 12;++i)
			initSet = initSet.crossWithSet(alphabet);
		
		// Second, we run them on both graphs
		
		PosNegPrecisionRecall bruteForcePR = computePrecisionRecall(graph, sequences);
		PTA_computePrecisionRecall precRec = new PTA_computePrecisionRecall(graph);
		precRec.crossWith(sequences);
		PosNegPrecisionRecall actualPR = precRec.getPosNegPrecisionRecallNum();
		
		// Third, we compare precision/recall computed in two different ways
		
		assertEquals("pos precision",bruteForcePR.getPosprecision(), actualPR.getPosprecision(),Configuration.fpAccuracy);
		assertEquals("pos recall",bruteForcePR.getPosrecall(), actualPR.getPosrecall(),Configuration.fpAccuracy);
		assertEquals("neg precision",bruteForcePR.getNegprecision(), actualPR.getNegprecision(),Configuration.fpAccuracy);
		assertEquals("neg recall",bruteForcePR.getNegrecall(), actualPR.getNegrecall(),Configuration.fpAccuracy);
	}
}
