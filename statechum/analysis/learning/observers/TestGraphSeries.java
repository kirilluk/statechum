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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.List;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.analysis.learning.rpnicore.GD;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;

import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
public class TestGraphSeries {
	LearnerGraph graphA = null, graphB = null, graphC = null, graphD = null;
	String xmlData = null;
	Collection<List<String>> plus, minus = null, justSomething = null;
	
	/** Graphs will have to be renumbered at all times. */ 
	Configuration renumberConfig = null;
	
	@Before
	public final void beforeTest()
	{
		renumberConfig = Configuration.getDefaultConfiguration().copy();
/*
		graphA = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-a->C", "TestGraphSeries1"),renumberConfig);
		graphB = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C", "TestGraphSeries2"),renumberConfig);
		graphC = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->D-b->D-a->C", "TestGraphSeries3"),renumberConfig);
		graphD = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->B-a->C-a-#D", "TestGraphSeries4"),renumberConfig);
		*/
		graphA = new LearnerGraph(TestFSMAlgo.buildGraph("A1-a->A1-b->B1-a->C1", "A_TestGraphSeries1"),renumberConfig);
		graphB = new LearnerGraph(TestFSMAlgo.buildGraph("A2-a->B2-a->C2", "B_TestGraphSeries2"),renumberConfig);
		graphC = new LearnerGraph(TestFSMAlgo.buildGraph("A3-a->D3-b->D3-a->C3", "C_TestGraphSeries3"),renumberConfig);
		graphD = new LearnerGraph(TestFSMAlgo.buildGraph("A4-a->B4-a->C4-a-#D4", "D_TestGraphSeries4"),renumberConfig);
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output, 1,renumberConfig,false);
		GraphSeries series = dumper.series;
		Element grElement = null;
		grElement = series.writeGraph(graphA);Assert.assertEquals(Transform.graphmlNodeNameNS, grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		grElement = series.writeGraph(graphB);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		grElement = series.writeGraph(graphC);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		grElement = series.writeGraph(graphD);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		dumper.close();
		xmlData = output.toString();
	}

	/** A helper method to check that the entire series can be read.
	 * 
	 * @param loader the loader to use.
	 * @param series the series of graphs to use
	 * @param checkTags whether to check tags of XML elements being read. 
	 */
	public final void readFourEntriesInSeries(LearnerSimulator loader,GraphSeries series, boolean checkTags)
	{
		LearnerGraph graph = null;
		Element elem = null;
		
		/** We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(Transform.graphmlNodeNameNS,elem.getNodeName());
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphA);Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(GD.ChangesRecorder.gdGD,elem.getNodeName());
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphB);Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(GD.ChangesRecorder.gdGD,elem.getNodeName());
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphC);Assert.assertEquals(graphC.getStateNumber(),graph.getStateNumber());

		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(GD.ChangesRecorder.gdGD,elem.getNodeName());
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphD);Assert.assertEquals(graphD.getStateNumber(),graph.getStateNumber());
	}
	
	/** One series, reset, once again, reset. */
	@Test
	public final void testReadMultipleSeries()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = renumberConfig;
		GraphSeries series = new GraphSeries(renumberConfig);
		LearnerGraph graph = null;
		Element elem = null;
		
		/* We read a part of the series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphA);Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphB);Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());

		elem = loader.getNextElement();Assert.assertNotNull(elem);		
		
		/* Now reset and read the rest. */
		series.reset();loader.childOfTopElement = 0;
		readFourEntriesInSeries(loader,series,true);
		elem = loader.getNextElement();Assert.assertNull(elem);		
		/* and again. */
		series.reset();loader.childOfTopElement = 0;
		readFourEntriesInSeries(loader,series,true);
		elem = loader.getNextElement();Assert.assertNull(elem);		
	}
	
	/** Write series, reset, write once again, then read it all back as a single series, reset and do it again. */
	@Test
	public final void testWriteMultipleSeries()
	{
		{// Writing part
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
			GraphSeries series = dumper.series;
			Element grElement = null;
			grElement = series.writeGraph(graphA);Assert.assertEquals(Transform.graphmlNodeNameNS, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
			grElement = series.writeGraph(graphB);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
			series.reset();
			grElement = series.writeGraph(graphC);Assert.assertEquals(Transform.graphmlNodeNameNS, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
			grElement = series.writeGraph(graphD);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
	
			grElement = series.writeGraph(graphA);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
			grElement = series.writeGraph(graphB);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
			dumper.close();
			xmlData = output.toString();
		}
		
		{// Reading part
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
			loader.config = renumberConfig;
			GraphSeries series = new GraphSeries(renumberConfig);
			LearnerGraph graph = null;
			Element elem = null;

			/* read the first four entries. */
			readFourEntriesInSeries(loader,series,false);
			elem = loader.getNextElement();Assert.assertNotNull(elem);		
			/* now reset and read them again. */
			series.reset();loader.childOfTopElement = 0;
			readFourEntriesInSeries(loader,series,false);

			/* We the rest of the series. */
			elem = loader.getNextElement();Assert.assertNotNull(elem);
			graph = series.readGraph(elem);
			WMethod.checkM(graph, graphA);Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
			
			elem = loader.getNextElement();Assert.assertNotNull(elem);
			graph = series.readGraph(elem);
			WMethod.checkM(graph, graphB);Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());
			elem = loader.getNextElement();Assert.assertNull(elem);		
			
			/* and again the first four. */
			series.reset();loader.childOfTopElement = 0;
			readFourEntriesInSeries(loader,series,false);
		}
	}

	@Test
	public final void testWriteMultipleSeries_long1()
	{
		tryLongSeries(Transform.convertToNumerical(graphA), 
				Transform.convertToNumerical(graphB), 
				Transform.convertToNumerical(graphC),
				Transform.convertToNumerical(graphD));
	}
	
	@Test
	public final void testWriteMultipleSeries_long2()
	{
		tryLongSeries(graphA,graphB,graphC,graphD);
	}
	
	/** A very long sequence of graphs. */
	public void tryLongSeries(LearnerGraph A,LearnerGraph B, LearnerGraph C, LearnerGraph D)
	{
		renumberConfig.setGdFailOnDuplicateNames(false);
		LearnerGraph graphs[]=new LearnerGraph[]{
				A,B,C,D, null};// null means "do reset".
		final int length = 202;
		{// Writing part
			Random rndGraphToUse = new Random(0);

			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
			GraphSeries series = dumper.series;

			for(int i=0;i<length;++i)
			{
				LearnerGraph graph = graphs[rndGraphToUse.nextInt(graphs.length)];
				if (graph == null) 
					series.reset();
				else
				{
					Element grElement = series.writeGraph(graph);
					dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(Transform.endl(dumper.doc));
				}
			}
			dumper.close();
			xmlData = output.toString();
		}
		
		{// Reading part
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
			loader.config = renumberConfig;
			GraphSeries series = new GraphSeries(renumberConfig);
			LearnerGraph graph = null;
			Random rndGraphToUse = new Random(0);
			Element elem = null;
			for(int i=0;i<length;++i)
			{
				LearnerGraph expected = graphs[rndGraphToUse.nextInt(graphs.length)];
				if (expected != null)
				{
					elem = loader.getNextElement();Assert.assertNotNull(elem);
					graph = series.readGraph(elem);
					WMethod.checkM(graph, expected);Assert.assertEquals(expected.getStateNumber(),graph.getStateNumber());
				}
			}
			elem = loader.getNextElement();Assert.assertNull(elem);		
		}
	}
	
	/** Attempt to write read-only series. */
	@Test
	public final void writeReadOnly1()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
		final GraphSeries series = dumper.series;
		Element grElement1 = series.writeGraph(graphA);Assert.assertEquals(Transform.graphmlNodeNameNS, grElement1.getNodeName());
		dumper.topElement.appendChild(grElement1);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		final Element grElement2 = series.writeGraph(graphB);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement2.getNodeName());
		dumper.topElement.appendChild(grElement2);dumper.topElement.appendChild(Transform.endl(dumper.doc));

		checkForCorrectException(new whatToRun() { public void run() {
			series.readGraph(grElement2);
		}},IllegalArgumentException.class,"write-only");
	}
	
	/** Attempt to write read-only series, reset does not change anything. */
	@Test
	public final void writeReadOnly2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
		final GraphSeries series = dumper.series;
		Element grElement1 = series.writeGraph(graphA);Assert.assertEquals(Transform.graphmlNodeNameNS, grElement1.getNodeName());
		dumper.topElement.appendChild(grElement1);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		final Element grElement2 = series.writeGraph(graphB);Assert.assertEquals(GD.ChangesRecorder.gdGD, grElement2.getNodeName());
		dumper.topElement.appendChild(grElement2);dumper.topElement.appendChild(Transform.endl(dumper.doc));
		
		series.reset();
		
		checkForCorrectException(new whatToRun() { public void run() {
			series.readGraph(grElement2);
		}},IllegalArgumentException.class,"write-only");
	}
	
	/** Attempt to read write-only series. */
	@Test
	public final void readWriteOnly1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = renumberConfig;
		final GraphSeries series = new GraphSeries(renumberConfig);
		LearnerGraph graph = null;
		Element elem = null;
		
		/** We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphA);Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		checkForCorrectException(new whatToRun() { public void run() {
			series.writeGraph(graphA);
		}},IllegalArgumentException.class,"read-only");
		
	}
	
	/** Attempt to read write-only series, reset does not change anything. */
	@Test
	public final void readWriteOnly2()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		loader.config = renumberConfig;
		final GraphSeries series = new GraphSeries(renumberConfig);
		LearnerGraph graph = null;
		Element elem = null;
		
		/** We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		WMethod.checkM(graph, graphA);Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		series.reset();
		
		checkForCorrectException(new whatToRun() { public void run() {
			series.writeGraph(graphA);
		}},IllegalArgumentException.class,"read-only");
		
	}
	
	/** Attempt to read from a non-Element. */
	@Test
	public final void readNonElement()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
		final GraphSeries series = new GraphSeries(renumberConfig);
		
		checkForCorrectException(new whatToRun() { public void run() {
			series.readGraph(Transform.endl(dumper.doc));
		}},IllegalArgumentException.class,"loadGraph was passed a non-element");
	}

	/** Attempt to read from an invalid element tag. */
	@Test
	public final void readInvalidElement()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,renumberConfig,false);
		final GraphSeries series = new GraphSeries(renumberConfig);

		checkForCorrectException(new whatToRun() { public void run() {
			series.readGraph(dumper.doc.createElement(TestRecordProgressDecorator.junkTag));
		}},IllegalArgumentException.class,"expected either graph or GD");
	}
}
