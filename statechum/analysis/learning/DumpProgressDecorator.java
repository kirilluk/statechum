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

import java.io.StringWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.List;
import java.util.Stack;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Element;

import statechum.JUConstants;
import statechum.analysis.learning.TestFSMAlgo.FSMStructure;
import statechum.analysis.learning.computeStateScores.PairScore;
import statechum.xmachine.model.testset.WMethod;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class DumpProgressDecorator extends ProgressDecorator {
	protected Writer outputWriter = null;
	
	public DumpProgressDecorator(Learner learner, Writer outWriter) 
	{
		super(learner);outputWriter = outWriter;

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
			topElement = doc.createElement(ELEM_KINDS.ELEM_STATECHUM_TESTTRACE.toString());doc.appendChild(topElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		}
		catch(ParserConfigurationException e)
		{
			throwUnchecked("failed to construct DOM document",e);
		}
	}

	@Override
	public void close()
	{
		try {
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(outputWriter));
			outputWriter.close();
		} catch (Exception e) {
			throwUnchecked("failed to write out XML ",e);
		}
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
		Element questionElement = doc.createElement(ELEM_KINDS.ELEM_ANSWER.toString());
		int result = decoratedLearner.CheckWithEndUser(graph, question, options);
		StringWriter strWriter = new StringWriter();writeInputSequence(strWriter,question);
		questionElement.setAttribute(ELEM_KINDS.ATTR_QUESTION.toString(),strWriter.toString());
		questionElement.setAttribute(ELEM_KINDS.ATTR_FAILEDPOS.toString(), Integer.toString(result));
		topElement.appendChild(questionElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		return result;
	}

	public Stack<PairScore> ChooseStatePairs(computeStateScores graph) {
		Stack<PairScore> result = decoratedLearner.ChooseStatePairs(graph);
		Element pairsElement = doc.createElement(ELEM_KINDS.ELEM_PAIRS.toString());
		for(PairScore p:result)
		{
			Element pair = doc.createElement(ELEM_KINDS.ELEM_PAIR.toString());
			pair.setAttribute(ELEM_KINDS.ATTR_Q.toString(), p.getQ().getUserDatum(JUConstants.LABEL).toString());
			pair.setAttribute(ELEM_KINDS.ATTR_R.toString(), p.getR().getUserDatum(JUConstants.LABEL).toString());
			pair.setAttribute(ELEM_KINDS.ATTR_SCORE.toString(), Integer.toString(p.getScore()));
			pairsElement.appendChild(pair);pairsElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		}
		topElement.appendChild(pairsElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		return result;
	}

	public Collection<List<String>> ComputeQuestions(
			computeStateScores original, computeStateScores temp, PairScore pair) 
	{
		Collection<List<String>> result = decoratedLearner.ComputeQuestions(original, temp, pair);
		Element questionList = addSequenceList(ELEM_KINDS.ELEM_QUESTIONS.toString(), result);
		topElement.appendChild(questionList);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		return result;
	}
	
	/** Stores the current learner input parameters. */
	public void handleLearnerEvaluationData(FSMStructure fsm, Collection<List<String>> testSet)
	{
		Element evaluationData = doc.createElement(ELEM_KINDS.ELEM_EVALUATIONDATA.toString());
		evaluationData.appendChild(FSMStructure.createGraphMLNode(fsm, doc));
		Element sequenceListElement = addSequenceList(ELEM_KINDS.ELEM_TESTSET.toString(), testSet);
		evaluationData.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		evaluationData.appendChild(sequenceListElement);evaluationData.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		topElement.appendChild(evaluationData);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));		
	}
	
	public computeStateScores MergeAndDeterminize(computeStateScores original,
			StatePair pair) 
	{
		computeStateScores result = decoratedLearner.MergeAndDeterminize(original, pair);
		//encoder.writeObject(FSMSTructure_PersistenceDelegate.getGraphAsXML(WMethod.getGraphData(result.getGraph())));//result.getGraph());
		return result;
	}

	/** We deliberately avoid storing this so as to be able to change 
	 * the format of diagnostics without having to regenerate test data. 
	 */
	public String getResult() 
	{
		String result = decoratedLearner.getResult();
		return result;
	}

	public DirectedSparseGraph init(Collection<List<String>> plus,
			Collection<List<String>> minus) 
	{
		DirectedSparseGraph result = decoratedLearner.init(plus, minus);
		topElement.appendChild(FSMStructure.createGraphMLNode(WMethod.getGraphData(result), doc));
		topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		return result;
	}

	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
		Element restartElement = doc.createElement(ELEM_KINDS.ELEM_RESTART.toString());
		restartElement.setAttribute(ELEM_KINDS.ATTR_KIND.toString(),mode.toString());
		topElement.appendChild(restartElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
	}
}
