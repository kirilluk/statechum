/** Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov

This file is part of StateChum.

statechum is free software: you can redistribute it and/or modify
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
package statechum.analysis.learning.observers;

import static statechum.Helper.checkForCorrectException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.TestFSMAlgo;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.analysis.learning.observers.ProgressDecorator.ELEM_KINDS;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.addExtraAttribute;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.breakNumericalValue;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;

/**
 * @author kirill
 *
 */
public class TestWriteReadPair {
	
	DirectedSparseGraph graph = null;
	String xmlData = null;
	
	@Before
	public final void beforeTest()
	{
		graph = TestFSMAlgo.buildGraph("A-a->B-a->C", "testWritePairs1");
		PairScore pair = new PairScore(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"A",graph),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"B",graph),6,7);
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(dumper.writePair(pair));dumper.close();
		
		xmlData = output.toString();
	}
	
	/** Tests writing a pair. */
	@Test
	public final void testWritePair1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		Assert.assertEquals(new PairScore(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"A",graph),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"B",graph),6,7),
				loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name())));
	}
	
	/** Tests writing a pair: extra attribute ignored. */
	@Test
	public final void testWritePair2()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(addExtraAttribute(xmlData,ELEM_KINDS.ELEM_PAIR).getBytes()),false);
		Assert.assertEquals(new PairScore(RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"A",graph),
				RPNIBlueFringeLearner.findVertex(JUConstants.LABEL,"B",graph),6,7),
				loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name())));
	}
	
	/** Failure reading a pair. */
	@Test
	public final void testWritePair_fail1()
	{
		final String wrongTag = "junk";
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.replace(ELEM_KINDS.ELEM_PAIR.name(), wrongTag).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(wrongTag));
		}},IllegalArgumentException.class,"expected to load a pair but got");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail2()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,ELEM_KINDS.ATTR_Q).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,ELEM_KINDS.ATTR_R).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail4()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,ELEM_KINDS.ATTR_SCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail5()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,ELEM_KINDS.ATTR_OTHERSCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Failure reading a pair: invalid integer. */
	@Test
	public final void testWritePair_fail6()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, ELEM_KINDS.ATTR_SCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"failed to read a score");
	}

	/** Failure reading a pair: invalid integer. */
	@Test
	public final void testWritePair_fail7()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, ELEM_KINDS.ATTR_OTHERSCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.readPair(graph, loader.expectNextElement(ELEM_KINDS.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"failed to read a anotherscore");
	}

}
