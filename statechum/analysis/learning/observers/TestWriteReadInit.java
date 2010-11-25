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

package statechum.analysis.learning.observers;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.StatechumXML;
import statechum.analysis.learning.observers.ProgressDecorator.InitialData;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.breakNumericalValue;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;

/**
 * @author kirill
 *
 */
public class TestWriteReadInit {
	LearnerGraph graph = null;
	String xmlData = null;
	Collection<List<String>> plus, minus = null, justSomething = null;
	
	@Before
	public final void beforeTest()
	{
		graph = new LearnerGraph(FsmParser.buildGraph("A-a->A-b->B-a->C", "TestWriteReadLearnerEvaluation"),Configuration.getDefaultConfiguration());
		plus = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{"more data"}
		});
		minus = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "t","some test data"},
				new String[]{ "q"},
				new String[]{},
				new String[]{"4","46"}
		});
		justSomething = TestFSMAlgo.buildList(new String[][]{
				new String[]{"a"},
				new String[]{"5"}
		});

		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();
	}

	@Test
	public final void createInitialData()
	{
		InitialData data = new InitialData(plus,5,minus,4,graph);
		Assert.assertEquals(5,data.plusSize);Assert.assertEquals(4, data.minusSize);
		Assert.assertSame(plus,data.plus);Assert.assertSame(minus,data.minus);Assert.assertSame(graph,data.graph);
	}
	
	@Test
	public final void createInitialData_fail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new InitialData(new LinkedList<List<String>>(),-1,new LinkedList<List<String>>(),0,null);
		}},IllegalArgumentException.class,"inconsistent positive");
	}
	
	@Test
	public final void createInitialData_fail2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			new InitialData(new LinkedList<List<String>>(),0,new LinkedList<List<String>>(),5,null);
		}},IllegalArgumentException.class,"inconsistent negative");
	}
	
	@Test
	public final void testLoadInit1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		InitialData data=loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		Assert.assertEquals(plus.size(),data.plusSize);Assert.assertEquals(minus.size(), data.minusSize);
		Assert.assertEquals(plus,data.plus);Assert.assertEquals(minus,data.minus);Assert.assertEquals(graph,data.graph);
	}

	/** Extra attributes are ignored. */
	@Test
	public final void testLoadInit2()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				xmlData
				.replace("<"+StatechumXML.ATTR_POSITIVE_SIZE.name(), "<"+StatechumXML.ATTR_POSITIVE_SIZE.name()+" AA=\"34\" ")
				.replace("<"+StatechumXML.ATTR_NEGATIVE_SIZE.name(), "<"+StatechumXML.ATTR_NEGATIVE_SIZE.name()+" AB=\"TT\" ")
				.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		InitialData data=loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		loader.config = Configuration.getDefaultConfiguration();
		Assert.assertEquals(plus.size(),data.plusSize);Assert.assertEquals(minus.size(), data.minusSize);
		Assert.assertEquals(plus,data.plus);Assert.assertEquals(minus,data.minus);Assert.assertEquals(graph,data.graph);
	}

	/** Missing initial tag. */
	@Test
	public final void testLoadInit_fail1()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(removeTagFromString(xmlData,StatechumXML.ELEM_INIT).getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(statechum.analysis.learning.observers.TestRecordProgressDecorator.junkTag));
		}},IllegalArgumentException.class,"expecting to load learner initial data");
	}
	
	/** Missing positive. */
	@Test
	public final void testLoadInit_fail2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		Element positiveToRemove = (Element)StatechumXML.getChildWithTag(initElement,StatechumXML.ELEM_SEQ.name()).item(0);
		initElement.removeChild(positiveToRemove);
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"missing positive");
	}
	
	/** Duplicate positive. */
	@Test
	public final void testLoadInit_fail3()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		initElement.appendChild(dumper.writeSequenceList(StatechumXML.ATTR_POSITIVE_SEQUENCES.name(), justSomething));
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"duplicate positive");
	}
	
	/** Missing negative. */
	@Test
	public final void testLoadInit_fail4()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		Element negativeToRemove = (Element)initElement.getElementsByTagName(StatechumXML.ELEM_SEQ.name()).item(1);// the second one
		initElement.removeChild(negativeToRemove);
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"missing negative");
	}
	
	/** Duplicate negative. */
	@Test
	public final void testLoadInit_fail5()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		initElement.appendChild(dumper.writeSequenceList(StatechumXML.ATTR_NEGATIVE_SEQUENCES.name(), justSomething));
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"duplicate negative");
	}
	
	/** Missing graph. */
	@Test
	public final void testLoadInit_fail6()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		Element graphToRemove = (Element)StatechumXML.getChildWithTag(initElement,StatechumXML.graphmlNodeNameNS.toString()).item(0);
		initElement.removeChild(graphToRemove);
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"missing graph");
	}
	
	/** Duplicate graph. */
	@Test
	public final void testLoadInit_fail7()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		initElement.appendChild(new LearnerGraph(FsmParser.buildGraph("A-a->A", "testLoadInit_fail7"),Configuration.getDefaultConfiguration())
			.storage.createGraphMLNode(dumper.doc));
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"duplicate graph");
	}
	
	/** Missing number of positive elements. */
	@Test
	public final void testLoadInit_fail8()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				removeTagFromString(xmlData,StatechumXML.ATTR_POSITIVE_SIZE).getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"missing positive size");
	}
	
	/** Invalid number of positive elements. */
	@Test
	public final void testLoadInit_fail9()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, StatechumXML.ATTR_POSITIVE_SIZE).getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"positive value is not");
	}
	
	/** Missing number of negative elements. */
	@Test
	public final void testLoadInit_fail10()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData
				.replace(StatechumXML.ATTR_NEGATIVE_SIZE.name(), "JUNKTAG").getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"missing negative size");
	}
	
	/** Invalid number of negative elements. */
	@Test
	public final void testLoadInit_fail11()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(
				breakNumericalValue(xmlData, StatechumXML.ATTR_NEGATIVE_SIZE).getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"negative value is not");
	}
	
	/** Completely unexpected element. */
	@Test
	public final void testLoadInit_fail12()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		initElement.appendChild(dumper.doc.createElement("junk"));
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"unexpected");
	}
	
	/** Completely unexpected element of a test sequence kind. */
	@Test
	public final void testLoadInit_fail13()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element initElement = dumper.writeInitialData(new InitialData(plus,plus.size(),minus,minus.size(),graph));
		Element testSequences = (Element)StatechumXML.getChildWithTag(initElement,StatechumXML.ELEM_SEQ.name()).item(0);
		testSequences.removeAttribute(StatechumXML.ATTR_SEQ.name());testSequences.setAttribute(StatechumXML.ATTR_SEQ.name(), "junk");
		dumper.topElement.appendChild(initElement);dumper.close();
		xmlData = output.toString();

		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = Configuration.getDefaultConfiguration();
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readInitialData(loader.expectNextElement(StatechumXML.ELEM_INIT.name()));
		}},IllegalArgumentException.class,"unexpected kind of sequences");
	}
}
