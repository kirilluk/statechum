/* Copyright (c) 2013 The University of Sheffield.
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
package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.DeterministicDirectedSparseGraph.VertID;
import statechum.Helper.whatToRun;
import statechum.analysis.Erlang.Synapse.StatechumProcess;
import statechum.analysis.learning.Visualiser.LayoutOptions;
import statechum.analysis.learning.experiments.mutation.DiffExperiments.MachineGenerator;
import statechum.analysis.learning.linear.DifferenceVisualiser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraphND;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.AMEquivalenceClass.IncompatibleStatesException;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod.DifferentFSMException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import edu.uci.ics.jung.graph.Edge;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestSynapseAuxiliaryFunctions {

	/** Deterministic case. */
	@Test
	public void testParseAutomataAndComputeDiff1() throws IncompatibleStatesException
	{
		ConvertALabel converter = null;
		Configuration config = Configuration.getDefaultConfiguration().copy();
		Random rnd=new Random(0);
		for(int states=1;states < 100;states++)
		{
			final int alphabet = 2*states;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			LearnerGraph graph = mg.nextMachine(alphabet,-states, config, converter).pathroutines.buildDeterministicGraph();
						
			LearnerGraph parsedOutcome = new LearnerGraph(config);
			StatechumProcess.parseStatemachine(StatechumProcess.constructFSM(graph), parsedOutcome, converter,true);
			DifferentFSMException diffException = WMethod.checkM(graph, parsedOutcome);
			Assert.assertNull(diffException);

			Map<VertID,VertID> map = new TreeMap<VertID,VertID>();
			for(int i=0;i<20;++i) map.put(graph.pathroutines.pickRandomState(rnd),graph.pathroutines.pickRandomState(rnd));
			Map<VertID,VertID> mapOutcome = StatechumProcess.parseMap(StatechumProcess.mapToObject(map));
			Assert.assertEquals(map,mapOutcome);
		}
	}
	
	/** Non-deterministic case. */
	@Test
	public void testParseAutomataAndComputeDiff2()
	{
		ConvertALabel converter = null;
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		Random rnd=new Random(0);
		LearnerGraphND previous = null;
		
		for(int states=1;states < 100;states++)
		{
			final int alphabet = 2*states;
			MachineGenerator mg = new MachineGenerator(states, 400 , (int)Math.round((double)states/5));mg.setGenerateConnected(true);
			LearnerGraphND graph = mg.nextMachine(alphabet,-states, config, converter);
			
			LearnerGraphND parsedOutcome = new LearnerGraphND(config);
			StatechumProcess.parseStatemachine(StatechumProcess.constructFSM(graph), parsedOutcome, converter,true);
			DifferentFSMException diffException = WMethod.checkM(graph, parsedOutcome);
			Assert.assertNull(diffException);

			Map<VertID,VertID> map = new TreeMap<VertID,VertID>();
			for(int i=0;i<20;++i) map.put(graph.pathroutines.pickRandomState(rnd),graph.pathroutines.pickRandomState(rnd));
			Map<VertID,VertID> mapOutcome = StatechumProcess.parseMap(StatechumProcess.mapToObject(map));
			Assert.assertEquals(map,mapOutcome);
			
			if (previous != null)
			{
				LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(graph,config);
				OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(graph, previous, config);
				DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
				DifferentFSMException ex = WMethod.checkM(previous, shouldBeLikePrevious);
				Assert.assertNull(ex);
			}
			previous = graph;
		}
	}
	
	
	@Test
	public void testSetRed1()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertEquals(0, grA.getRedStateNumber());
		Synapse.StatechumProcess.setReds(new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("P1002")}), grA);
		Assert.assertEquals(JUConstants.RED,grA.findVertex("P1002").getColour());
		Assert.assertEquals(1, grA.getRedStateNumber());
	}
	
	@Test
	public void testSetRed2()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		final LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertEquals(0, grA.getRedStateNumber());
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Synapse.StatechumProcess.setReds(new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("P1002")}), grA);
		}},ClassCastException.class,"OtpErlangTuple");
	}
	
	@Test
	public void testSetRed3()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		final LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertEquals(0, grA.getRedStateNumber());
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Synapse.StatechumProcess.setReds(new OtpErlangList(new OtpErlangObject[]{new OtpErlangString("P1002")}), grA);
		}},ClassCastException.class,"OtpErlangString");
	}
	
	@Test
	public void testSetRed4()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		final LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertEquals(0, grA.getRedStateNumber());
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Synapse.StatechumProcess.setReds(new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("P1009")}), grA);
		}},IllegalArgumentException.class,"not found");
	}
	
	/** Special case of fsmdiff */
	@Test
	public void testFsmDiff1a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertTrue(grA.getInit().isAccept());
		Assert.assertFalse(grA.findVertex("N1000").isAccept());
		grA.findVertex("N1000").setAccept(true);
		
		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1000'],"+
              "[{'P1000',a,'P1001'},"+
              "{'P1000',c,'P1001'},"+
              "{'P1001',a,'N1000'},"+
              "{'P1001',b,'P1000'},"+
              "{'P1001',d,'P1001'}],"+
              "'P1000',"+
              "[d,b,c,a]}"),grB,null,true);
		Assert.assertTrue(grB.getInit().isAccept());
		Assert.assertFalse(grB.findVertex("N1000").isAccept());
		grB.findVertex("N1000").setAccept(true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1003','a','P1001'},"+
		                                             "{'P1003','c','P1001'},"+
		                                             "{'P1001','a','P1000'},"+
		                                             "{'P1001','b','P1003'},"+
		                                             "{'P1001','d','P1001'}],"+
		                                            "[{'P1000','a','P1000'},"+
		                                            "{'P1000','b','P1002'},"+
		                                             "{'P1000','c','N1000'},"+
		                                             "{'P1002','c','P1002'},"+
		                                             "{'P1002','d','P1002'}],"+
		                                            "['P1001','P1003'],"+
		                                           "['N1000','P1002'],"+
		                                            "[{'P1000','N1000'},{'P1003','P1000'}],"+
		                                            "'P1003'}" , ErlangLabel.dumpErlangObject(difference));
		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(shouldBeLikePrevious.getStateNumber(),grB.getStateNumber());
		
		DirectedSparseGraph gr = DifferenceVisualiser.ChangesToGraph.computeVisualisationParameters(grAerlang, difference);
		Map<String,String> edgeToColours = new TreeMap<String,String>();
		for(Object edgeObj:gr.getEdges())
		{
			Edge edge = (Edge)edgeObj;
			if (edge.containsUserDatumKey(JUConstants.DIFF))
				edgeToColours.put(edge.toString(), edge.getUserDatum(JUConstants.DIFF).toString());
		}
		Assert.assertEquals("{P1000-[a]->P1000=java.awt.Color[r=255,g=0,b=0], P1000-[b]->P1002=java.awt.Color[r=255,g=0,b=0], P1000-[c]->N1000=java.awt.Color[r=255,g=0,b=0], P1001-[a]->P1000=java.awt.Color[r=0,g=255,b=0], P1001-[b]->P1003=java.awt.Color[r=0,g=255,b=0], P1001-[d]->P1001=java.awt.Color[r=0,g=255,b=0], P1002-[c, d]->P1002=java.awt.Color[r=255,g=0,b=0], P1003-[a, c]->P1001=java.awt.Color[r=0,g=255,b=0]}",
				edgeToColours.toString());
	}
	
	/** Special case of fsmdiff */
	@Test
	public void testFsmDiff1b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000',a,'P1000'},"+
	            " {'P1000',b,'P1002'},"+
	            " {'P1000',c,'N1000'},"+
	            " {'P1002',c,'P1002'},"+
	            " {'P1002',d,'P1002'}],"+
	            "'P1000',"+
	            "[d,b,c,a]}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);
		Assert.assertTrue(grA.getInit().isAccept());
		Assert.assertFalse(grA.findVertex("N1000").isAccept());
		
		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1000'],"+
              "[{'P1000',a,'P1001'},"+
              "{'P1000',c,'P1001'},"+
              "{'P1001',a,'N1000'},"+
              "{'P1001',b,'P1000'},"+
              "{'P1001',d,'P1001'}],"+
              "'P1000',"+
              "[d,b,c,a]}"),grB,null,true);
		Assert.assertTrue(grB.getInit().isAccept());
		Assert.assertFalse(grB.findVertex("N1000").isAccept());
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1000','a','P1002'},{'P1000','c','P1002'},{'P1002','a','N1000'},{'P1002','b','P1000'}],[{'P1000','a','P1000'},{'P1000','b','P1002'},{'P1000','c','N1000'},{'P1002','c','P1002'}],[],[],[{'P1002','P1001'}],'P1000'}" , ErlangLabel.dumpErlangObject(difference));
	}
	
	/** Special case of fsmdiff - disconnected states */
	@Test
	public void testFsmDiff2()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1000'],"+
              "[{'P1000','a','P1001'}],"+
              "'P1000',"+
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1000','a','P1001'}],[{'P1000','a','P1000'}],['N1000','P1001'],['P1002'],[],'P1000'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}
	
	/** Special case of fsmdiff - disconnected states with different names */
	@Test
	public void testFsmDiff3a()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1001'],"+
              "[{'P1000','a','P1001'}],"+
              "'P1000',"+
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1000','a','P1001'}],[{'P1000','a','P1000'}],['N1001','P1001'],['N1000','P1002'],[],'P1000'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}
	
	/** Special case of fsmdiff - disconnected states with different names */
	@Test
	public void testFsmDiff3b()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1001'],"+
              "[{'P1001','a','P1001'}],"+
              "'P1001',"+
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[],[],['N1001','P1003'],['N1000','P1002'],[{'P1000','P1001'},{'P1003','P1000'}],'P1000'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}
	
	/** Special case of fsmdiff - disconnected states with different names */
	@Test
	public void testFsmDiff3c()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1001'],"+
              "[{'P1001','a','P1001'}],"+
              "'P1000',"+ // a disconnected initial state
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[],[],['N1001','P1003'],['N1000','P1002'],[{'P1000','P1001'},{'P1003','P1000'}],'P1003'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}
	
	/** Special case of fsmdiff - disconnected states */
	@Test
	public void testFsmDiff4()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001','N1000'],"+
              "[{'P1000','a','P1001'}],"+
              "'P1000',"+
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1000','a','P1001'}],[{'P1000','a','P1000'}],['N1000','P1001'],['P1002'],[],'P1000'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}
	
	/** Special case of fsmdiff - disconnected states */
	@Test
	public void testFsmDiff5()
	{
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setGdFailOnDuplicateNames(false);
		LearnerGraphND grA = new LearnerGraphND(config);
		OtpErlangObject grAerlang = ErlangLabel.parseText("{statemachine,['P1000','P1002','N1000'],"+
	            "[{'P1000','a','P1000'}],"+
	            "'P1000',"+
	            "['a']}");
		Synapse.StatechumProcess.parseStatemachine(grAerlang,grA,null,true);

		LearnerGraphND grB = new LearnerGraphND(config);
		Synapse.StatechumProcess.parseStatemachine(ErlangLabel.parseText("{statemachine,['P1000','P1001'],"+
              "[{'P1000','a','P1001'}],"+
              "'P1000',"+
              "['a']}"),grB,null,true);
		
		OtpErlangObject difference = DifferenceVisualiser.ChangesToGraph.computeGD(grA, grB, config);
		Assert.assertEquals("{'statemachinedifference',[{'P1000','a','P1001'}],[{'P1000','a','P1000'}],['P1001'],['N1000','P1002'],[],'P1000'}",ErlangLabel.dumpErlangObject(difference));

		LearnerGraphND shouldBeLikePrevious = new LearnerGraphND(grA,config);
		DifferenceVisualiser.ChangesToGraph.load(difference).applyDiff(shouldBeLikePrevious, config);
		DifferentFSMException ex = WMethod.checkM(grB, shouldBeLikePrevious);
		Assert.assertNull(ex);Assert.assertEquals(grB.getStateNumber(),shouldBeLikePrevious.getStateNumber());
	}

	@Test
	public void testLayoutOptions1()
	{
		final LayoutOptions options = new LayoutOptions();
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangTuple(new OtpErlangObject[0]));
		}},ClassCastException.class,"OtpErlangTuple");// when we do not collect output, no response is sent.
		
	}
	
	@Test
	public void testLayoutOptions2()
	{
		final LayoutOptions options = new LayoutOptions();
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangString("a")}));
		}},ClassCastException.class,"OtpErlangString");// when we do not collect output, no response is sent.
		
	}

	@Test
	public void testLayoutOptions3()
	{
		final LayoutOptions options = new LayoutOptions();
		
		Assert.assertNull(options.ignoredStates);
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[0]));
		Assert.assertNull(options.ignoredStates);
	}

	@Test
	public void testLayoutOptions4()
	{
		final LayoutOptions options = new LayoutOptions();
		
		Assert.assertNull(options.ignoredStates);
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("a")}));
		Assert.assertNotNull(options.ignoredStates);
		Assert.assertEquals(1,options.ignoredStates.size());Assert.assertEquals("a",options.ignoredStates.iterator().next());
		
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("b"),new OtpErlangAtom("a")}));
		Assert.assertNotNull(options.ignoredStates);
		Assert.assertEquals(2,options.ignoredStates.size());
		Iterator<String> ignoreIterator = options.ignoredStates.iterator();
		Assert.assertEquals("a",ignoreIterator.next());Assert.assertEquals("b",ignoreIterator.next());
	}


	@Test
	public void testLayoutOptions5()
	{
		final LayoutOptions options = new LayoutOptions();
		
		Assert.assertNull(options.ignoredStates);
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("b"),new OtpErlangAtom("a")}));
		Assert.assertNotNull(options.ignoredStates);
		Assert.assertEquals(2,options.ignoredStates.size());
		Iterator<String> ignoreIterator = options.ignoredStates.iterator();
		Assert.assertEquals("a",ignoreIterator.next());Assert.assertEquals("b",ignoreIterator.next());
	}
	
	/** Tests the copying process for ignored states. */
	@Test
	public void testLayoutOptions6()
	{
		final LayoutOptions options = new LayoutOptions();
		
		Assert.assertNull(options.ignoredStates);
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("b"),new OtpErlangAtom("a")}));
		Assert.assertNotNull(options.ignoredStates);
		Assert.assertEquals(2,options.ignoredStates.size());
		Iterator<String> ignoreIterator = options.ignoredStates.iterator();
		Assert.assertEquals("a",ignoreIterator.next());Assert.assertEquals("b",ignoreIterator.next());
		
		LayoutOptions copy = options.copy();
		Synapse.StatechumProcess.setStateNamesToBeIgnored(options, new OtpErlangList(new OtpErlangObject[]{new OtpErlangAtom("c")}));
		Assert.assertNotNull(options.ignoredStates);
		Assert.assertEquals(3,options.ignoredStates.size());
		ignoreIterator = options.ignoredStates.iterator();
		Assert.assertEquals("a",ignoreIterator.next());Assert.assertEquals("b",ignoreIterator.next());Assert.assertEquals("c",ignoreIterator.next());
		options.showIgnored = !options.showIgnored;
		
		// now check the original is unchanged.
		Assert.assertEquals(2,copy.ignoredStates.size());
		ignoreIterator = copy.ignoredStates.iterator();
		Assert.assertEquals("a",ignoreIterator.next());Assert.assertEquals("b",ignoreIterator.next());
		Assert.assertEquals(!options.showIgnored,copy.showIgnored);
	}

	
	
}
