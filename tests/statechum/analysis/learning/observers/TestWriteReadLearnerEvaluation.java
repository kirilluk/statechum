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
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.ParameterizedWithName;
import org.junit.runners.ParameterizedWithName.ParametersToString;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.Label;
import statechum.StatechumXML;
import statechum.Configuration.LABELKIND;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.smt.SmtLabelRepresentation;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.apps.QSMTool;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;
import static statechum.analysis.learning.smt.SmtLabelRepresentation.INITMEM;
import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;

/**
 * @author kirill
 *
 */
@RunWith(ParameterizedWithName.class)
public class TestWriteReadLearnerEvaluation {
	Configuration config = null, anotherconfig = null;
	ConvertALabel converter = null;
	
	LearnerGraph graph = null;
	String xmlData = null;
	Collection<List<Label>> testData = null;
	Collection<String> ltl = null;
	SmtLabelRepresentation labels = null;
	
	@org.junit.runners.Parameterized.Parameters
	public static Collection<Object[]> data() 
	{
		List<Object[]> outcome = new LinkedList<Object[]>();
		outcome.add(new Object[]{false,LABELKIND.LABEL_ERLANG});
		outcome.add(new Object[]{false,LABELKIND.LABEL_STRING});
		outcome.add(new Object[]{true,LABELKIND.LABEL_STRING});
		return outcome;
	}
	
	private final LABELKIND labelKind;
	private final boolean legacy;
	
	/** Creates the test class with the number of threads to create as an argument. */
	public TestWriteReadLearnerEvaluation(Boolean legacyArg, LABELKIND labelKindArg)
	{
		legacy = legacyArg;labelKind = labelKindArg;
	}
	
	@ParametersToString
	public static String parametersToString(Boolean legacyArg, LABELKIND labelKind)
	{
		return (legacyArg.booleanValue()?"Legacy":"Current")+" "+labelKind;
	}

	public String lbl(String l)
	{
		return AbstractLearnerGraph.inventParsableLabel(l, config);
	}
	
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();config.setLabelKind(labelKind);config.setLegacyXML(legacy);
		converter = null;
		
		double defaultK = config.getAttenuationK();
		config.setAttenuationK(0.333);// make sure this configuration is different from the default one.
		Assert.assertFalse(config.getAttenuationK() == defaultK);// and make sure that the two are indeed not the same.
		anotherconfig = Configuration.getDefaultConfiguration().copy();anotherconfig.setLabelKind(labelKind);anotherconfig.setLegacyXML(legacy);
		Assert.assertFalse(config.equals(anotherconfig));
		
		graph = FsmParser.buildLearnerGraph("A-"+lbl("a")+"->A-"+lbl("b")+"->B-"+lbl("a")+"->C", "TestWriteReadLearnerEvaluation",config,converter);
		testData = TestFSMAlgo.buildList(new String[][]{
				new String[]{ lbl("a"),lbl("this is a test"),lbl("3")},
				new String[]{},
				new String[]{lbl("more data")}
		},config,converter);
		ltl = Arrays.asList(new String[] { 
				"![](setfiletype -> X((storefile) || (rename)))",
				"ltl ![]((initialise) -> X(connect))",
				"ltl !(XX(initialise))" });
		labels = new SmtLabelRepresentation(config,converter);
		labels.parseCollection(Arrays.asList(new String[]{
			QSMTool.cmdOperation+" "+lbl(INITMEM)+" "+SmtLabelRepresentation.OP_DATA.PRE+ " varDecl_N",
			QSMTool.cmdOperation+" "+lbl(INITMEM)+" "+SmtLabelRepresentation.OP_DATA.PRE+ " initCond_N",
			QSMTool.cmdOperation+" "+lbl("A")+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondA",
			QSMTool.cmdOperation+" "+lbl("A")+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondA",
			QSMTool.cmdOperation+" "+lbl("B")+" "+SmtLabelRepresentation.OP_DATA.PRE+ " somePrecondB",
			QSMTool.cmdOperation+" "+lbl("B")+" "+SmtLabelRepresentation.OP_DATA.POST+ " somePostcondB"}));

		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		dumper.topElement.appendChild(dumper.writeLearnerEvaluationConfiguration(
				new LearnerEvaluationConfiguration(graph,testData,config,ltl,labels)));dumper.close();
		
		xmlData = output.toString();
	}
	
	/** Success. */
	@Test
	public final void testLearnerEvaluation1()
	{
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
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
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,Configuration.configXMLTag).item(0);
		learnerConfig.removeChild(configToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),anotherconfig);
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);
		Assert.assertEquals(anotherconfig, cnf.config);
		Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertEquals(labels,cnf.labelDetails);
	}
	
	/** A completely unexpected element is ignored. */
	@Test
	public final void testLearnerEvaluation9()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,config,ltl,labels));
		learnerConfig.appendChild(dumper.doc.createElement("junk"));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
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
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
			Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_CONSTRAINTS.name()).item(0);
			org.w3c.dom.Node crlf = configToRemove.getNextSibling();
			learnerConfig.removeChild(configToRemove);learnerConfig.removeChild(crlf);
			
			dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		}

		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,null,labels));
			dumper.topElement.appendChild(learnerConfig);dumper.close();anotherXML = output.toString();
		}
		Assert.assertEquals(anotherXML,xmlData);
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
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
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
			Element configToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_LABELDETAILS.name()).item(0);
			org.w3c.dom.Node crlf = configToRemove.getNextSibling();
			learnerConfig.removeChild(configToRemove);learnerConfig.removeChild(crlf);
			
			dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		}

		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,null));
			dumper.topElement.appendChild(learnerConfig);dumper.close();anotherXML = output.toString();
		}
		Assert.assertEquals(anotherXML,xmlData);
		
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		LearnerEvaluationConfiguration cnf=loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ifthenSequences);
		Assert.assertNull(cnf.labelDetails);
	}

	/** Missing main element. */
	@Test
	public final void testLearnerEvaluation3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(removeTagFromString(xmlData,StatechumXML.ELEM_EVALUATIONDATA).getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(statechum.analysis.learning.observers.TestRecordProgressDecorator.junkTag),config);
		}},IllegalArgumentException.class,"expecting to load learner evaluation data");
	}
	
	/** Missing graph. */
	@Test
	public final void testLearnerEvaluation4()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element graphToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.graphmlNodeNameNS.toString()).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"missing graph");
	}	

	/** Duplicate graph. */
	@Test
	public final void testLearnerEvaluation5()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(FsmParser.buildLearnerGraph("A-"+lbl("a")+"->A", "testLoadInit_fail7",config,converter)
		.storage.createGraphMLNode(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"duplicate graph");
	}	
	
	/** Missing test set. */
	@Test
	public final void testLearnerEvaluation6()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		Element graphToRemove = (Element)StatechumXML.getChildWithTag(learnerConfig,StatechumXML.ELEM_SEQ.name()).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"missing test set");
	}	
	
	/** Duplicate test set. */
	@Test
	public final void testLearnerEvaluation7()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(dumper.labelio.writeSequenceList(StatechumXML.ATTR_POSITIVE_SEQUENCES.name(), 
				TestFSMAlgo.buildList(new String[][]{
						new String[]{ lbl("t"),lbl("some test data")},
						new String[]{},
						new String[]{lbl("4"),lbl("46")}},config,converter)));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"duplicate test set");
	}	
	
	/** Duplicate configuration. */
	@Test
	public final void testLearnerEvaluation8()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(anotherconfig.writeXML(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"duplicate configuration");
	}	
	
	/** Duplicate label definition. */
	@Test
	public final void testLearnerEvaluation12()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl,labels));
		learnerConfig.appendChild(labels.storeToXML(dumper.doc,dumper.stringio));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(xmlData.getBytes()),false,converter);
		checkForCorrectException(new whatToRun() { public @Override void run() {
			loader.readLearnerEvaluationConfiguration(loader.expectNextElement(StatechumXML.ELEM_EVALUATIONDATA.name()),config);
		}}, IllegalArgumentException.class,"duplicate label details");
	}	
	
}
