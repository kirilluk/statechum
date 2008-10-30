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

package statechum.analysis.learning;

import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.model.testset.PTASequenceEngine;
import statechum.model.testset.PTASequenceSetAutomaton;
import statechum.model.testset.PTASequenceEngine.DebugDataValues;
import statechum.model.testset.PTASequenceEngine.SequenceSet;

import edu.uci.ics.jung.graph.Vertex;
import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class TestPTAConstruction
{
	@Test
	public void testAugmentPTA() // only two traces, both accept
	{
		Set<List<String>> plusStrings = buildSet(new String[][] { new String[] {"a","b","c"},new String[]{"a","d","c"} });
		DirectedSparseGraph actualA = Test_Orig_RPNIBlueFringeLearner.augmentPTA(DeterministicDirectedSparseGraph.initialise(), plusStrings, true),
			actualC = null;
		DeterministicDirectedSparseGraph.numberVertices(actualA);// Numbering is necessary to ensure uniqueness of labels used by LearnerGraph constructor.
		Configuration config = Configuration.getDefaultConfiguration().copy();config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
		config.setAllowedToCloneNonCmpVertex(true);
		LearnerGraph l = new LearnerGraph(config);
		actualC = l.paths.augmentPTA(plusStrings, true).paths.getGraph();
		DeterministicDirectedSparseGraph.numberVertices(actualA);
		String expectedPTA = "A-a->B--b->C-c->End1\nB--d->C2-c->End2";
		TestFSMAlgo.checkM(actualA, expectedPTA,config);
		TestFSMAlgo.checkM(actualC, expectedPTA,config);
	}

	private static PTASequenceEngine buildPTA(Set<List<String>> plusStrings,Set<List<String>> minusStrings)
	{
		final Boolean accept = new Boolean(true), reject = new Boolean(false);
		boolean theOnlyStateReject = false;
		for(List<String> seq:minusStrings)
			if (seq.isEmpty())
			{
				theOnlyStateReject = true;break;
			}
		final Boolean rejectAllStates = new Boolean(theOnlyStateReject);
		final AtomicBoolean statesAccept = new AtomicBoolean(true);
		PTASequenceEngine allSequences = new PTASequenceEngine();
		allSequences.init(new PTASequenceSetAutomaton()
		{
			@Override
			public Object getTheOnlyState() {
				return statesAccept.get()?accept:reject;
			}
			@Override
			public boolean shouldBeReturned(Object elem) {
				return elem != null && ((Boolean)elem).booleanValue();
			}
			@Override
			public boolean isAccept(@SuppressWarnings("unused")	Object elem)
			{
				return !rejectAllStates.booleanValue();
			}
		});
		SequenceSet initSeq = allSequences.new SequenceSet();initSeq.setIdentity();
		initSeq.cross(plusStrings);statesAccept.getAndSet(false);initSeq.cross(minusStrings);
		
		return allSequences;
	}

	/** Checks if a PTA constructed is consistent with provided sequences. */
	private void checkPTAConsistency(PTASequenceEngine engine,Set<List<String>> sequences, boolean accept)
	{
		SequenceSet initSeq = engine.new SequenceSet();initSeq.setIdentity();
		// Now we check the consistency
		for(List<String> seq:sequences)
		{
			SequenceSet endOfSeq = initSeq.crossWithSequence(seq);
			Map<String,String> map = engine.getDebugDataMapDepth(endOfSeq);
			assert map.size() == 1: "expected size of 1, got "+map.size();
			String attrs = map.values().iterator().next();
			// For reject-sequences,
			// If the end of the sequence is not a leaf, it should be considered positive, hence throw.
			// If the end of the sequence is a leaf but should be returned, throw.
			// The only remaining case is that the end is a leaf and should not be returned - this is ok.
			if (!accept && !attrs.equals(DebugDataValues.booleanToString(true,false)))
				throw new IllegalArgumentException("reject-sequence "+seq+" is present in PTA with a positive ending");
			// For accept-sequences, the only erroneous case is when a leaf is not returned.
			// (we assume that non-leaf is always returned.) 
			if (accept && attrs.equals(DebugDataValues.booleanToString(true,false)))
				throw new IllegalArgumentException("reject-sequence "+seq+" is present in PTA with a negative ending");
		}		
	}
	
	private void checkEmptyPTA(String[][] arrayPlusStrings,String [][] arrayMinusStrings)
	{
		Set<List<String>> plusStrings = buildSet(arrayPlusStrings), minusStrings = buildSet(arrayMinusStrings);
		DirectedSparseGraph actualA = null, actualC = null, actualD = null, actualE = null;
		IllegalArgumentException eA = null, eC = null, eD = null, eE = null;
		try
		{
			actualA = Test_Orig_RPNIBlueFringeLearner.createAugmentedPTA(plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}

		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			PTASequenceEngine engine = buildPTA(plusStrings, minusStrings);
			checkPTAConsistency(engine, plusStrings, true);if (engine.numberOfLeafNodes()>0) checkPTAConsistency(engine, minusStrings, false);
			l.init(engine,0,0);
			actualD = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eD = e;
		}

		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(buildPTA(plusStrings, buildSet(new String[][] {})),0,0);
			for(List<String> seq:minusStrings)
				l.scoreComputer.paths.augmentPTA(buildPTA(buildSet(new String[][] {}),buildSet(new String[][] { (String [])seq.toArray()})));
			actualE = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eE = e;
		}

		if (eA != null)
		{
			Assert.assertNotNull(eC);
			Assert.assertNotNull(eD);
			Assert.assertNotNull(eE);
			throw eA;
		}
		
		Assert.assertNull(eA);
		Assert.assertNull(eC);
		Assert.assertNull(eD);
		Assert.assertNull(eE);
				
		Assert.assertEquals(1, actualA.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualA.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualA.getEdges().size());

		Assert.assertEquals(1, actualC.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualC.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualC.getEdges().size());

		Assert.assertEquals(1, actualD.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualD.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualD.getEdges().size());

		Assert.assertEquals(1, actualE.getVertices().size());Assert.assertEquals(true, DeterministicDirectedSparseGraph.isAccept( ((Vertex)actualD.getVertices().iterator().next()) )); 
		Assert.assertEquals(0, actualE.getEdges().size());
	}
	
	/** An empty accept trace. */
	@Test
	public void testPTAconstruction1a()
	{
		checkEmptyPTA(
				new String[][] { new String[]{}},
				new String[][] { }
			);
	}

	/** An empty reject trace. */
	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction1b()
	{
		checkEmptyPTA(
				new String[][] { },
				new String[][] { new String[]{} }
			);
	}

	/** Empty traces. */
	@Test
	public void testPTAconstruction1c()
	{
		checkEmptyPTA(
				new String[][] {},
				new String[][] {}
			);
	}

	/** Builds a PTA from the supplied arguments using two different methods. If any of them throws, checks that another one throws too and then rethrows the exception. 
	 * 
	 * @param arrayPlusStrings allowed sequences
	 * @param arrayMinusStrings sequences ending at a reject state
	 * @param expectedPTA a textual representation of a PTA which should be built.
	 */
	private void checkPTAconstruction(String[][] arrayPlusStrings,String [][] arrayMinusStrings, String expectedPTA)
	{
		Set<List<String>> plusStrings = buildSet(arrayPlusStrings), minusStrings = buildSet(arrayMinusStrings);
		DirectedSparseGraph actualA = null, actualC =null, actualD = null, actualE = null;
		IllegalArgumentException eA = null, eC = null, eD = null, eE = null;
		try
		{
			actualA = Test_Orig_RPNIBlueFringeLearner.createAugmentedPTA(plusStrings, minusStrings);
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eA = e;
		}

		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(plusStrings, minusStrings);
			actualC = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eC = e;
		}
		
		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			PTASequenceEngine engine = buildPTA(plusStrings, minusStrings);
			checkPTAConsistency(engine, plusStrings, true);if (engine.numberOfLeafNodes()>0) checkPTAConsistency(engine, minusStrings, false);
			l.init(engine,0,0);
			actualD = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eD = e;
		}

		try
		{
			Configuration config = Configuration.getDefaultConfiguration().copy();
			RPNIUniversalLearner l = new RPNIUniversalLearner(null,new LearnerEvaluationConfiguration(null,null,config,null,null));
			config.setLearnerIdMode(Configuration.IDMode.POSITIVE_NEGATIVE);
			l.init(buildPTA(plusStrings, buildSet(new String[][] {})),0,0);
			for(List<String> seq:minusStrings)
				l.scoreComputer.paths.augmentPTA(buildPTA(buildSet(new String[][] {}),buildSet(new String[][] { (String [])seq.toArray()})));
			actualE = l.scoreComputer.paths.getGraph();
		}
		catch(IllegalArgumentException e)
		{
			// ignore this - it might be expected.
			eE = e;
		}

		if (eA != null)
		{
			Assert.assertNotNull(eC);
			Assert.assertNotNull(eD);
			Assert.assertNotNull(eE);
			throw eA;
		}

		Assert.assertNull(eA);
		Assert.assertNull(eC);
		Assert.assertNull(eD);
		Assert.assertNull(eE);

		Configuration config = Configuration.getDefaultConfiguration().copy();
		config.setAllowedToCloneNonCmpVertex(true);
		TestFSMAlgo.checkM(actualA, expectedPTA,config);
		TestFSMAlgo.checkM(actualC, expectedPTA,config);
		//Visualiser.updateFrame(actualE,TestFSMAlgo.buildGraph(expectedPTA,"expected graph"));Visualiser.waitForKey();
		TestFSMAlgo.checkM(actualD,expectedPTA ,config);
		TestFSMAlgo.checkM(actualE,expectedPTA ,config);
	}
	
	@Test
	public void testPTAconstruction2()// two accept traces and one reject one
	{
		checkPTAconstruction(
			new String[][] { new String[]{"a","b","c"}, new String[]{"a","d","c"}},
			new String[][] { new String[]{"a","b","c","d"} },
			"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2"
			);
	}

	@Test
	public void testPTAconstruction3()// two accept traces and two reject ones
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, new String[]{"a","u"} },
				"A-a->B--b->C-c->End1-d-#REJ\nB--d->C2-c->End2\nB-u-#A2");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction4()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b"} },
				"junk");
	}


	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction5()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c"} },
				"junk");
	}


	@Test(expected = IllegalArgumentException.class)
	public void testPTAconstruction6()// a trace goes through a reject-state
	{
		checkPTAconstruction(
				new String[][] { new String[]{"a","b","c"}, new String[]{"a","b"}, new String[]{"a","d","c"}},
				new String[][] { new String[]{"a","b","c","d"}, 
						new String[]{"a","b","c","d","e"} },
				"junk");
	}
}
