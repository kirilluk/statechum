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

import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Element;

import statechum.Configuration;
import statechum.Helper.whatToRun;
import statechum.analysis.learning.observers.ProgressDecorator.ELEM_KINDS;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import static statechum.analysis.learning.observers.TestRecordProgressDecorator.removeTagFromString;
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
	Collection<List<String>> testData = null;
	Collection<String> ltl = null;
	
	@Before
	public final void beforeTest()
	{
		config = Configuration.getDefaultConfiguration().copy();
		config.setAttenuationK(0.333);// make sure this configuration is different from the default one.
		anotherconfig = Configuration.getDefaultConfiguration().copy();
		Assert.assertFalse(config.equals(anotherconfig));
		
		graph = new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A-b->B-a->C", "TestWriteReadLearnerEvaluation"),config);
		testData = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{"more data"}
		});
		ltl = Arrays.asList(new String[] { 
				"![](setfiletype -> X((storefile) || (rename)))",
				"ltl ![]((initialise) -> X(connect))",
				"ltl !(XX(initialise))" });
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		dumper.topElement.appendChild(dumper.writeLearnerEvaluationConfiguration(
				new LearnerEvaluationConfiguration(graph,testData,config,ltl)));dumper.close();
		
		xmlData = output.toString();
	}
	
	/** Success. */
	@Test
	public final void testLearnerEvaluation1()
	{
		LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		LearnerEvaluationConfiguration cnf=ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);
		Assert.assertEquals(config, cnf.config);
		Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ltlSequences);
	}
	
	/** Missing configuration. */
	@Test
	public final void testLearnerEvaluation2()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		Element configToRemove = (Element)learnerConfig.getElementsByTagName(Configuration.configXMLTag).item(0);
		learnerConfig.removeChild(configToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		LearnerEvaluationConfiguration cnf=ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ltlSequences);
	}
	
	/** A completely unexpected element is ignored. */
	@Test
	public final void testLearnerEvaluation9()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,config,ltl));
		learnerConfig.appendChild(dumper.doc.createElement("junk"));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		LearnerEvaluationConfiguration cnf=ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);
		Assert.assertEquals(config, cnf.config);
		Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertEquals(ltl,cnf.ltlSequences);
	}	

	/** Missing ltl part. */
	@Test
	public final void testLearnerEvaluation10()
	{
		String anotherXML = null;
		{
			StringWriter output = new StringWriter();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
			Element configToRemove = (Element)learnerConfig.getElementsByTagName(ELEM_KINDS.ELEM_LTL.name()).item(0);
			org.w3c.dom.Node crlf = configToRemove.getNextSibling();
			learnerConfig.removeChild(configToRemove);learnerConfig.removeChild(crlf);
			
			dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		}

		{
			StringWriter output = new StringWriter();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
			Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,null));
			dumper.topElement.appendChild(learnerConfig);dumper.close();anotherXML = output.toString();
		}
		Assert.assertEquals(anotherXML,xmlData);
		
		LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		LearnerEvaluationConfiguration cnf=ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		WMethod.checkM(cnf.graph, graph);
		Assert.assertEquals(testData, cnf.testSet);Assert.assertEquals(anotherconfig, cnf.config);Assert.assertEquals(cnf.config, cnf.graph.config);
		Assert.assertNull(cnf.ltlSequences);
	}
	
	/** Missing main element. */
	@Test
	public final void testLearnerEvaluation3()
	{
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(removeTagFromString(xmlData,ELEM_KINDS.ELEM_EVALUATIONDATA)));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(statechum.analysis.learning.observers.TestRecordProgressDecorator.junkTag));
		}},IllegalArgumentException.class,"expecting to load learner evaluation data");
	}
	/** Missing graph. */
	@Test
	public final void testLearnerEvaluation4()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		Element graphToRemove = (Element)learnerConfig.getElementsByTagName(Transform.graphmlNodeName).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"missing graph");
	}	

	/** Duplicate graph. */
	@Test
	public final void testLearnerEvaluation5()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		learnerConfig.appendChild(new LearnerGraph(TestFSMAlgo.buildGraph("A-a->A", "testLoadInit_fail7"),Configuration.getDefaultConfiguration())
		.transform.createGraphMLNode(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate graph");
	}	
	
	/** Missing test set. */
	@Test
	public final void testLearnerEvaluation6()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		Element graphToRemove = (Element)learnerConfig.getElementsByTagName(ELEM_KINDS.ELEM_SEQ.name()).item(0);
		learnerConfig.removeChild(graphToRemove);
		
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"missing test set");
	}	
	
	/** Duplicate test set. */
	@Test
	public final void testLearnerEvaluation7()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		learnerConfig.appendChild(dumper.writeSequenceList(ELEM_KINDS.ATTR_POSITIVE_SEQUENCES.name(), 
				TestFSMAlgo.buildList(new String[][]{
						new String[]{ "t","some test data"},
						new String[]{},
						new String[]{"4","46"}})));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate test set");
	}	
	
	/** Duplicate configuration. */
	@Test
	public final void testLearnerEvaluation8()
	{
		StringWriter output = new StringWriter();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration());
		Element learnerConfig = dumper.writeLearnerEvaluationConfiguration(new LearnerEvaluationConfiguration(graph,testData,anotherconfig,ltl));
		learnerConfig.appendChild(anotherconfig.writeXML(dumper.doc));
		dumper.topElement.appendChild(learnerConfig);dumper.close();xmlData = output.toString();
		
		final LearnerSimulator loader = new LearnerSimulator(new StringReader(xmlData));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readLearnerEvaluationConfiguration(loader.expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.name()));
		}}, IllegalArgumentException.class,"duplicate configuration");
	}	
	
}
