/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum.
 * 
 * statechum is free software: you can redistribute it and/or modify
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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.Label;
import statechum.StatechumXML;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LabelRepresentation;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.apps.QSMTool;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;
import static statechum.analysis.learning.rpnicore.LabelRepresentation.INITMEM;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
public class TestWriteReadLearnerEvaluation {
	Configuration config = null, anotherconfig = null;
	LearnerGraph graph = null;
	String xmlData = null;
	Collection<List<Label>> testData = null;
	Collection<String> ltl = null;
	LabelRepresentation labels = null;
	
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();
		double defaultK = config.getAttenuationK();
		config.setAttenuationK(0.333);// make sure this configuration is different from the default one.
		Assert.assertFalse(config.getAttenuationK() == defaultK);// and make sure that the two are indeed not the same.
		anotherconfig = Configuration.getDefaultConfiguration().copy();
		Assert.assertFalse(config.equals(anotherconfig));
		
		graph = FsmParser.buildLearnerGraph("A-a->A-b->B-a->C", "TestWriteReadLearnerEvaluation",config);
		testData = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{"more data"}
		},config);
		ltl = Arrays.asList(new String[] { 
				"![](setfiletype -> X((storefile) || (rename)))",
				"ltl ![]((initialise) -> X(connect))",
				"ltl !(XX(initialise))" });
		labels = new LabelRepresentation(config);
		labels.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " varDecl_N",
			QSMTool.cmdOperation+" "+INITMEM+" "+LabelRepresentation.OP_DATA.PRE+ " initCond_N",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			QSMTool.cmdOperation+" "+"A"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondA",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.PRE+ " somePrecondB",
			QSMTool.cmdOperation+" "+"B"+" "+LabelRepresentation.OP_DATA.POST+ " somePostcondB"}));

		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(dumper.writeLearnerEvaluationConfiguration(
				new LearnerEvaluationConfiguration(graph,testData,config,ltl,labels)));dumper.close();
		
		xmlData = output.toString();
	}
	
	/** Success. */
	@Test
	public final void testLearnerEvaluation1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);
		Assert.assertEquals(config, cnf.config);
		Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertEquals(labels,cnf.labelDetails);
	}
	
	/** Missing configuration. */
	@Test
	public final void testLearnerEvaluation2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,Configuration.configXMLTag).item(0);
		learnerConfig.removeChild(configToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertEquals(labels,cnf.labelDetails);
	}
	
	/** A completely unexpected element is ignored. */
	@Test
	public final void testLearnerEvaluation9()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,config,ltl,labels));
		learnerConfig.appendChild(dumper.doc.createElement("junk"));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);
		Assert.assertEquals(config, cnf.config);
		Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertEquals(labels,cnf.labelDetails);
	}	

	/** Missing LTL part. */
	@Test
	public final void testLearnerEvaluation10()
	{
		String anotherXML = null;
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
			Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_CONSTRAINTS.name()).item(0);
			org.w3c.dom.Node crlf = configToRemove.getNextSibling();
			learnerConfig.removeChild(configToRemove);learnerConfig.removeChild(crlf);
			
			dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		}

		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,null,labels));
			dumper.topElement.appendChild(learnerConfig);dumper.close();anotherXML = output.toString();
		}
		Assert.assertEquals(anotherXML,xmlData);
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(labels,cnf.labelDetails);
		Assert.assertNull(cnf.ifthenSequences);
	}
	
	/** Missing part with labels. */
	@Test
	public final void testLearnerEvaluation11()
	{
		String anotherXML = null;
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
			Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_LABELDETAILS.name()).item(0);
			org.w3c.dom.Node crlf = configToRemove.getNextSibling();
			learnerConfig.removeChild(configToRemove);learnerConfig.removeChild(crlf);
			
			dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		}

		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,null));
			dumper.topElement.appendChild(learnerConfig);dumper.close();anotherXML = output.toString();
		}
		Assert.assertEquals(anotherXML,xmlData);
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertNull(cnf.labelDetails);
	}

	/** Missing main element. */
	@Test
	public final void testLearnerEvaluation3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(removeTagFromString(xmlData,StatechumXML.ELEM_EVALUATIONDATA).getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(statechum.analysis.learning.observers.TestRecordProgressDecorator.junkTag));
		}},IllegalArgumentException.class,"expecting to load learner evaluation data");
	}
	
	/** Missing graph. */
	@Test
	public final void testLearnerEvaluation4()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element graphToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.graphmlNodeNameNS.toString()).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"missing graph");
	}	

	/** Duplicate graph. */
	@Test
	public final void testLearnerEvaluation5()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(FsmParser.buildLearnerGraph("A-a->A", "testLoadInit_fail7",Configuration.getDefaultConfiguration())
		.storage.createGraphMLNode(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate graph");
	}	
	
	/** Missing test set. */
	@Test
	public final void testLearnerEvaluation6()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element graphToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_SEQ.name()).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"missing test set");
	}	
	
	/** Duplicate test set. */
	@Test
	public final void testLearnerEvaluation7()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(dumper.labelio.writeSequenceList(StatechumXML.ATTR_POSITIVE_SEQUENCES.name(), 
				TestFSMAlgo.buildList(new String[][]{
						new String[]{ "t","some test data"},
						new String[]{},
						new String[]{"4","46"}},config)));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate test set");
	}	
	
	/** Duplicate configuration. */
	@Test
	public final void testLearnerEvaluation8()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(anotherconfig.writeXML(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate configuration");
	}	
	
	/** Duplicate label definition. */
	@Test
	public final void testLearnerEvaluation12()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(labels.storeToXML(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate label details");
	}	
	
}
