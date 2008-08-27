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

import java.io.Reader;
import java.util.Collection;
import java.util.List;
import java.util.Stack;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.Assert;
import org.w3c.dom.Element;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class MatchProgressDecorator extends ProgressDecorator {
	protected int childOfTopElement =0;

	/** Extracts next element from the collection of children of the top-level one.
	 * Text nodes with whitespace are ignored.
	 * 
	 * @param name expected name, exception otherwise
	 * @return element
	 */ 
	public Element expectNextElement(String name)
	{
		org.w3c.dom.Node result = null;
		do
		{
			org.junit.Assert.assertTrue(childOfTopElement<topElement.getChildNodes().getLength());
			result = topElement.getChildNodes().item(childOfTopElement++);
		}
		while(result.getNodeType() == org.w3c.dom.Node.TEXT_NODE);
		org.junit.Assert.assertEquals(name, result.getNodeName());
		return (Element)result;
	}
	
	public MatchProgressDecorator(Learner learner, Reader inputReader) 
	{
		super(learner);

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().parse(new org.xml.sax.InputSource(inputReader));
			topElement = doc.getDocumentElement();
		}
		catch(Exception e)
		{
			ProgressDecorator.throwUnchecked("failed to construct/load DOM document",e);
		}
	}
	
	@Override
	public DirectedSparseGraph learnMachine(@SuppressWarnings("unused")	Learner top_level_decorator)
	{
		return decoratedLearner.learnMachine(this);
	}
	
	@Override
	public DirectedSparseGraph learnMachine()
	{
		DirectedSparseGraph graph = learnMachine(this);
		return graph;
	}

	public int CheckWithEndUser(computeStateScores graph,
			List<String> question, Object[] options) 
	{
		Element questionElement = expectNextElement(ELEM_KINDS.ELEM_ANSWER.toString());
		int result = decoratedLearner.CheckWithEndUser(graph, question, options);
		List<String> expectedQuestion = readInputSequence(new java.io.StringReader(questionElement.getAttribute(ELEM_KINDS.ATTR_QUESTION.toString())),-1);
		Assert.assertEquals(expectedQuestion,question);
		Assert.assertEquals(questionElement.getAttribute(ELEM_KINDS.ATTR_FAILEDPOS.toString()), Integer.toString(result));
		return result;
	}

	public Stack<PairScore> ChooseStatePairs(computeStateScores graph) {
		Stack<PairScore> result = decoratedLearner.ChooseStatePairs(graph);
		Element pairsElement = expectNextElement(ELEM_KINDS.ELEM_PAIRS.toString());
		org.w3c.dom.Node tentativePair = pairsElement.getFirstChild();
		int currentChild=0;
		for(PairScore p:result)
		{
			do
			{
				org.junit.Assert.assertTrue(currentChild<pairsElement.getChildNodes().getLength());
				tentativePair = pairsElement.getChildNodes().item(currentChild++);
			}
			while(tentativePair.getNodeType() == org.w3c.dom.Node.TEXT_NODE);
			org.junit.Assert.assertEquals(ELEM_KINDS.ELEM_PAIR.toString(), tentativePair.getNodeName());
			Element pair = (Element)tentativePair;
			Assert.assertEquals(pair.getAttribute(ELEM_KINDS.ATTR_Q.toString()), p.getQ().getUserDatum(JUConstants.LABEL).toString());
			Assert.assertEquals(pair.getAttribute(ELEM_KINDS.ATTR_R.toString()), p.getR().getUserDatum(JUConstants.LABEL).toString());
			Assert.assertEquals(pair.getAttribute(ELEM_KINDS.ATTR_SCORE.toString()), Integer.toString(p.getScore()));
			pairsElement.appendChild(pair);pairsElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		}
		return result;
	}

	public List<List<String>> ComputeQuestions(
			computeStateScores original, computeStateScores temp, PairScore pair) {
		List<List<String>> result = decoratedLearner.ComputeQuestions(original, temp, pair);
		Element questionList = expectNextElement(ELEM_KINDS.ELEM_SEQ.toString());
		Collection<List<String>> expectedQuestions = readSequenceList(questionList,ELEM_KINDS.ELEM_QUESTIONS.toString());
		Assert.assertEquals(expectedQuestions, result);
		return result;
	}

	/** Checks the current learner input parameters. */
	public void handleLearnerEvaluationData(FSMStructure fsm, Collection<List<String>> testSet)
	{
		Element evaluationData = expectNextElement(ELEM_KINDS.ELEM_EVALUATIONDATA.toString());
		Element graphElement = (Element)evaluationData.getElementsByTagName(FSMStructure.graphmlNS+":graphml").item(0);// should not be more than one of these
		FSMStructure expected = WMethod.getGraphData(FSMStructure.loadGraph(graphElement));
		TestFSMAlgo.checkM(fsm, expected, fsm.init, expected.init);

		Element sequenceListElement = (Element)evaluationData.getElementsByTagName(ELEM_KINDS.ELEM_SEQ.toString()).item(0);// should not be more than one of these
		Collection<List<String>> expectedQuestions = readSequenceList(sequenceListElement,ELEM_KINDS.ELEM_TESTSET.toString());
		Assert.assertEquals(expectedQuestions, testSet);
	}

	public computeStateScores MergeAndDeterminize(computeStateScores original,
			StatePair pair) {
		computeStateScores result = decoratedLearner.MergeAndDeterminize(original, pair);
		//encoder.writeObject(FSMSTructure_PersistenceDelegate.getGraphAsXML(WMethod.getGraphData(result.getGraph())));//result.getGraph());
		return result;
	}

	public void Restart(RestartLearningEnum mode) {
		decoratedLearner.Restart(mode);
		Element restartElement = expectNextElement(ELEM_KINDS.ELEM_RESTART.toString());
		Assert.assertEquals(restartElement.getAttribute(ELEM_KINDS.ATTR_KIND.toString()),mode.toString());
	}

	/** We deliberately avoid storing this so as to be able to change 
	 * the format of diagnostics without having to regenerate test data. 
	 */
	public String getResult() 
	{
		String result = decoratedLearner.getResult();
		return result;
	}

	public DirectedSparseGraph init(Collection<List<String>> plus, Collection<List<String>> minus) {
		DirectedSparseGraph result = decoratedLearner.init(plus, minus);
		FSMStructure actual = WMethod.getGraphData(result);
		Element initGraphElement = expectNextElement(FSMStructure.graphmlNS+":graphml");
		FSMStructure expected = WMethod.getGraphData(FSMStructure.loadGraph(initGraphElement));
		TestFSMAlgo.checkM(actual, expected, actual.init, expected.init);
		return result;
	}
}
