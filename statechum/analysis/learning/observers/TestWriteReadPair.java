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

package statechum.analysis.learning.observers;

import static statechum.Helper.checkForCorrectException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.JUConstants;
import statechum.StatechumXML;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.addExtraAttribute;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.breakNumericalValue;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;

/**
 * @author kirill
 *
 */
public class TestWriteReadPair {
	
	LearnerGraph graph = null;
	String xmlData = null;
	
	/** Dumps a supplied pair into an XML and returns a string of text corresponding to the XML element produced. For testing only.
	 * 
	 * @param pair what to dump
	 * @return result of representing the supplied pair in XML.
	 */
	private static String pairToXMLDocument(PairScore pair)
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(ProgressDecorator.writePair(pair,dumper.doc));dumper.close();
		
		return output.toString();
	}
	
	public static String pairToXML(PairScore pair)
	{
		String otherScore = pair.getAnotherScore() == JUConstants.intUNKNOWN?"":" "+StatechumXML.ATTR_OTHERSCORE.name()+"=\""+Integer.toString(pair.getAnotherScore())+"\"";
		String mainScore = pair.getScore() == JUConstants.intUNKNOWN?"":" "+StatechumXML.ATTR_SCORE.name()+"=\""+Integer.toString(pair.getScore())+"\"";
		return "< "+StatechumXML.ELEM_PAIR+otherScore+" "+
			StatechumXML.ATTR_Q.name()+"=\""+pair.getQ().getID().toString()+"\" "+
			StatechumXML.ATTR_R.name()+"=\""+pair.getR().getID().toString()+"\""+
			mainScore+"/>";
	}
	
	@Before
	public final void beforeTest()
	{
		graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C", "testWritePairs1"),Configuration.getDefaultConfiguration());
		xmlData=pairToXMLDocument(new PairScore(graph.findVertex("A"),graph.findVertex("B"),6,7));
	}
	
	/** Tests writing a pair. */
	@Test
	public final void testWritePair1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		Assert.assertEquals(new PairScore(graph.findVertex("A"),graph.findVertex("B"),6,7),
				ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name())));
	}
	
	/** Tests writing a pair: extra attribute ignored. */
	@Test
	public final void testWritePair2()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(addExtraAttribute(xmlData,StatechumXML.ELEM_PAIR).getBytes()),false);
		Assert.assertEquals(new PairScore(graph.findVertex("A"),graph.findVertex("B"),6,7),
				ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name())));
	}
	
	/** Failure reading a pair. */
	@Test
	public final void testWritePair_fail1()
	{
		final String wrongTag = "junk";
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.replace(StatechumXML.ELEM_PAIR.name(), wrongTag).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readPair(graph, loader.expectNextElement(wrongTag));
		}},IllegalArgumentException.class,"expected to load a pair but got");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail2()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,StatechumXML.ATTR_Q).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Failure reading a pair: missing attribute. */
	@Test
	public final void testWritePair_fail3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,StatechumXML.ATTR_R).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"missing attribute");
	}

	/** Reading a pair: missing attribute means the intUnknown value will be read. */
	@Test
	public final void testWritePair_missing_score()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,StatechumXML.ATTR_SCORE).getBytes()),false);
		PairScore result = ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		PairScore expected = new PairScore(graph.findVertex("A"),graph.findVertex("B"),JUConstants.intUNKNOWN,7);
		Assert.assertEquals(expected,result);
	}

	/** Reading a pair: missing attribute means the intUnknown value will be read. */
	@Test
	public final void testWritePair_missing_otherscore()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,StatechumXML.ATTR_OTHERSCORE).getBytes()),false);
		PairScore result = ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		PairScore expected = new PairScore(graph.findVertex("A"),graph.findVertex("B"),6,JUConstants.intUNKNOWN);
		Assert.assertEquals(expected,result);
	}

	/** Failure reading a pair: invalid integer. */
	@Test
	public final void testWritePair_fail6()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, StatechumXML.ATTR_SCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"failed to read a score");
	}

	/** Failure reading a pair: invalid integer. */
	@Test
	public final void testWritePair_fail7()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, StatechumXML.ATTR_OTHERSCORE).getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readPair(graph, loader.expectNextElement(StatechumXML.ELEM_PAIR.name()));
		}},IllegalArgumentException.class,"failed to read a anotherscore");
	}

}
