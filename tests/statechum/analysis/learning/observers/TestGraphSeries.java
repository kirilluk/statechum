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
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import junit_runners.ParameterizedWithName;
import junit_runners.ParameterizedWithName.ParametersToString;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.StatechumXML;
import statechum.analysis.learning.linear.TestGDMultithreaded;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestWithMultipleConfigurations;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import static statechum.TestHelper.checkForCorrectException;

/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestGraphSeries extends TestWithMultipleConfigurations
{
	LearnerGraph graphA = null, graphB = null, graphC = null, graphD = null;
	String xmlData = null;

	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		return TestWithMultipleConfigurations.data();
	}
	
	@ParametersToString
	public static String parametersToString(Configuration config)
	{
		return TestWithMultipleConfigurations.parametersToString(config);
	}
	
	public TestGraphSeries(Configuration config)
	{
		super(config);
	}
	
	@Before
	public final void beforeTest()
	{
/*
		graphA = buildLearnerGraph("A-a->A-b->B-a->C", "TestGraphSeries1",mainConfiguration);
		graphB = buildLearnerGraph("A-a->B-a->C", "TestGraphSeries2",mainConfiguration);
		graphC = buildLearnerGraph("A-a->D-b->D-a->C", "TestGraphSeries3",mainConfiguration);
		graphD = buildLearnerGraph("A-a->B-a->C-a-#D", "TestGraphSeries4",mainConfiguration);
		*/
		graphA = buildLearnerGraph("A1-a->A1-b->B1-a->C1", "A_TestGraphSeries1",mainConfiguration,converter);
		graphB = buildLearnerGraph("A2-a->B2-a->C2", "B_TestGraphSeries2",mainConfiguration,converter);
		graphC = buildLearnerGraph("A3-a->D3-b->D3-a->C3", "C_TestGraphSeries3",mainConfiguration,converter);
		graphD = buildLearnerGraph("A4-a->B4-a->C4-a-#D4", "D_TestGraphSeries4",mainConfiguration,converter);
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(output, 1,mainConfiguration,false,converter);
		GraphSeries series = dumper.series;
		Element grElement = null;
		grElement = series.writeGraph(graphA);Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(), grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		grElement = series.writeGraph(graphB);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		grElement = series.writeGraph(graphC);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		grElement = series.writeGraph(graphD);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
		dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
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
		
		/* We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(),elem.getNodeName());
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphA,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(StatechumXML.gdGD.toString(),elem.getNodeName());
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphB,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(StatechumXML.gdGD.toString(),elem.getNodeName());
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphC,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphC.getStateNumber(),graph.getStateNumber());

		elem = loader.getNextElement();Assert.assertNotNull(elem);if (checkTags) Assert.assertEquals(StatechumXML.gdGD.toString(),elem.getNodeName());
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphD,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphD.getStateNumber(),graph.getStateNumber());
	}
	
	/** One series, reset, once again, reset. */
	@Test
	public final void testReadMultipleSeries()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = mainConfiguration;
		GraphSeries series = new GraphSeries(mainConfiguration,converter);
		LearnerGraph graph = null;
		Element elem = null;
		
		/* We read a part of the series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphA,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphB,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());

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
			RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
			GraphSeries series = dumper.series;
			Element grElement = null;
			grElement = series.writeGraph(graphA);Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
			grElement = series.writeGraph(graphB);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
			series.reset();
			grElement = series.writeGraph(graphC);Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
			grElement = series.writeGraph(graphD);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
	
			grElement = series.writeGraph(graphA);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
			grElement = series.writeGraph(graphB);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement.getNodeName());
			dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
			dumper.close();
			xmlData = output.toString();
		}
		
		{// Reading part
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
			loader.config = mainConfiguration;
			GraphSeries series = new GraphSeries(mainConfiguration,converter);
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
			Assert.assertNull(WMethod.checkM_and_colours(graph, graphA,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
			
			elem = loader.getNextElement();Assert.assertNotNull(elem);
			graph = series.readGraph(elem);
			Assert.assertNull(WMethod.checkM_and_colours(graph, graphB,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphB.getStateNumber(),graph.getStateNumber());
			elem = loader.getNextElement();Assert.assertNull(elem);		
			
			/* and again the first four. */
			series.reset();loader.childOfTopElement = 0;
			readFourEntriesInSeries(loader,series,false);
		}
	}

	@Test
	public final void testWriteMultipleSeries_long1()
	{
		tryLongSeries(TestGDMultithreaded.convertToNumerical(graphA),
				TestGDMultithreaded.convertToNumerical(graphB),
				TestGDMultithreaded.convertToNumerical(graphC),
				TestGDMultithreaded.convertToNumerical(graphD));
	}
	
	@Test
	public final void testWriteMultipleSeries_long2()
	{
		tryLongSeries(graphA,graphB,graphC,graphD);
	}
	
	/** A very long sequence of graphs. */
	public void tryLongSeries(LearnerGraph A,LearnerGraph B, LearnerGraph C, LearnerGraph D)
	{
		mainConfiguration.setGdFailOnDuplicateNames(false);
		LearnerGraph[] graphs =new LearnerGraph[]{
				A,B,C,D, null};// null means "do reset".
		final int length = 202;
		{// Writing part
			Random rndGraphToUse = new Random(0);

			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
			GraphSeries series = dumper.series;

			for(int i=0;i<length;++i)
			{
				LearnerGraph graph = graphs[rndGraphToUse.nextInt(graphs.length)];
				if (graph == null) 
					series.reset();
				else
				{
					Element grElement = series.writeGraph(graph);
					dumper.topElement.appendChild(grElement);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
				}
			}
			dumper.close();
			xmlData = output.toString();
		}
		
		{// Reading part
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
			loader.config = mainConfiguration;
			GraphSeries series = new GraphSeries(mainConfiguration,converter);
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
					Assert.assertNull(WMethod.checkM_and_colours(graph, expected,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(expected.getStateNumber(),graph.getStateNumber());
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
		RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
		final GraphSeries series = dumper.series;
		Element grElement1 = series.writeGraph(graphA);Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(), grElement1.getNodeName());
		dumper.topElement.appendChild(grElement1);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		final Element grElement2 = series.writeGraph(graphB);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement2.getNodeName());
		dumper.topElement.appendChild(grElement2);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));

		checkForCorrectException(
				() -> series.readGraph(grElement2),IllegalArgumentException.class,"write-only");
	}
	
	/** Attempt to write read-only series, reset does not change anything. */
	@Test
	public final void writeReadOnly2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
		final GraphSeries series = dumper.series;
		Element grElement1 = series.writeGraph(graphA);Assert.assertEquals(StatechumXML.graphmlNodeNameNS.toString(), grElement1.getNodeName());
		dumper.topElement.appendChild(grElement1);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		final Element grElement2 = series.writeGraph(graphB);Assert.assertEquals(StatechumXML.gdGD.toString(), grElement2.getNodeName());
		dumper.topElement.appendChild(grElement2);dumper.topElement.appendChild(AbstractPersistence.endl(dumper.doc));
		
		series.reset();
		
		checkForCorrectException(
				() -> series.readGraph(grElement2),IllegalArgumentException.class,"write-only");
	}
	
	/** Attempt to read write-only series. */
	@Test
	public final void readWriteOnly1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = mainConfiguration;
		final GraphSeries series = new GraphSeries(mainConfiguration,converter);
		LearnerGraph graph = null;
		Element elem = null;
		
		/* We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphA,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		checkForCorrectException(
				() -> series.writeGraph(graphA),IllegalArgumentException.class,"read-only");
		
	}
	
	/** Attempt to read write-only series, reset does not change anything. */
	@Test
	public final void readWriteOnly2()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		loader.config = mainConfiguration;
		final GraphSeries series = new GraphSeries(mainConfiguration,converter);
		LearnerGraph graph = null;
		Element elem = null;
		
		/* We read the entire series. */
		elem = loader.getNextElement();Assert.assertNotNull(elem);
		graph = series.readGraph(elem);
		Assert.assertNull(WMethod.checkM_and_colours(graph, graphA,VERTEX_COMPARISON_KIND.DEEP));Assert.assertEquals(graphA.getStateNumber(),graph.getStateNumber());
		
		series.reset();
		
		checkForCorrectException(
				() -> series.writeGraph(graphA),IllegalArgumentException.class,"read-only");
		
	}
	
	/** Attempt to read from a non-Element. */
	@Test
	public final void readNonElement()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
		final GraphSeries series = new GraphSeries(mainConfiguration,converter);
		
		checkForCorrectException(
				() -> series.readGraph(AbstractPersistence.endl(dumper.doc)),IllegalArgumentException.class,"loadGraph was passed a non-element");
	}

	/** Attempt to read from an invalid element tag. */
	@Test
	public final void readInvalidElement()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(output,1,mainConfiguration,false,converter);
		final GraphSeries series = new GraphSeries(mainConfiguration,converter);

		checkForCorrectException(
				() -> series.readGraph(dumper.doc.createElement(TestRecordProgressDecorator.junkTag)),IllegalArgumentException.class,"expected either graph or GD");
	}
}
