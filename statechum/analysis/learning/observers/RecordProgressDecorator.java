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

package statechum.analysis.learning.observers;

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

import statechum.Configuration;
import statechum.JUConstants;
import statechum.Pair;
import statechum.analysis.learning.PairScore;
import statechum.analysis.learning.StatePair;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.model.testset.PTASequenceEngine;

/** Stores some arguments and results of calls to learner's methods 
 * so that one could later test if a particular learner's behaviour 
 * exactly matches that of a known learner. 
 *
 * @author kirill
 */
public class RecordProgressDecorator extends ProgressDecorator {
	protected Writer outputWriter = null;
	
	/** The top-most element of the trace log file. */
	protected Element topElement = null;
	
	/** Graph compressor. */
	protected GraphSeries series = null;
	
	public RecordProgressDecorator(Learner learner, Writer outWriter, int threadNumber, Configuration conf) 
	{
		super(learner);outputWriter = outWriter;config = conf;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().newDocument();
			topElement = doc.createElement(ELEM_KINDS.ELEM_STATECHUM_TESTTRACE.name());doc.appendChild(topElement);topElement.appendChild(Transform.endl(doc));
			series = new GraphSeries(doc,threadNumber,config);
		}
		catch(ParserConfigurationException e)
		{
			statechum.Helper.throwUnchecked("failed to construct DOM document",e);
		}
	}

	/** Writes the outcome of learning into the log and closes the log.
	 * 
	 * @param graph the outcome of learning.
	 */
	protected void writeResult(LearnerGraph graph)
	{
		Element finalGraphXMLNode = series.writeGraph(graph);
		finalGraphXMLNode.setAttribute(ELEM_KINDS.ATTR_GRAPHKIND.name(),ELEM_KINDS.ATTR_LEARNINGOUTCOME.name());
		topElement.appendChild(finalGraphXMLNode);topElement.appendChild(Transform.endl(doc));
		close();
	}

	public double getCompressionRate()
	{
		return series.getCompressionRate();
	}
	
	/** Closes the trace log, writing the constructed XML out. */ 
	public void close()
	{
		try 
		{
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(outputWriter));
			outputWriter.close();
		} catch (Exception e) {
			statechum.Helper.throwUnchecked("failed to write out XML ",e);
		}
		finally
		{
			doc=null;topElement=null;			
		}
		
	}
	
	@Override
	public LearnerGraph learnMachine(final PTASequenceEngine engine, int plusSize, int minusSize)
	{
		LearnerGraph graph = decoratedLearner.learnMachine(engine, plusSize, minusSize);
		writeResult(graph);
		return graph;
	}
	
	@Override
	public LearnerGraph learnMachine(Collection<List<String>> plus,	Collection<List<String>> minus)
	{
		LearnerGraph graph = decoratedLearner.learnMachine(plus,minus);
		writeResult(graph);
		return graph;
	}
	
	public Pair<Integer,String> CheckWithEndUser(LearnerGraph graph,
			List<String> question, Object[] options) 
	{
		Element questionElement = doc.createElement(ELEM_KINDS.ELEM_ANSWER.name());
		Pair<Integer,String> result = decoratedLearner.CheckWithEndUser(graph, question, options);
		StringWriter strWriter = new StringWriter();writeInputSequence(strWriter,question);
		questionElement.setAttribute(ELEM_KINDS.ATTR_QUESTION.name(),strWriter.toString());
		questionElement.setAttribute(ELEM_KINDS.ATTR_FAILEDPOS.name(), result.firstElem.toString());
		if (result.secondElem != null) questionElement.setAttribute(ELEM_KINDS.ATTR_LTL.name(), result.secondElem);
		topElement.appendChild(questionElement);topElement.appendChild(Transform.endl(doc));
		return result;
	}

	public Stack<PairScore> ChooseStatePairs(LearnerGraph graph) {
		Stack<PairScore> result = decoratedLearner.ChooseStatePairs(graph);
		Element pairsElement = doc.createElement(ELEM_KINDS.ELEM_PAIRS.name());
		for(PairScore p:result)
		{
			pairsElement.appendChild(writePair(p));pairsElement.appendChild(Transform.endl(doc));
		}
		topElement.appendChild(pairsElement);topElement.appendChild(Transform.endl(doc));
		return result;
	}
	
	public Collection<List<String>> ComputeQuestions(PairScore pair, LearnerGraph original, LearnerGraph temp) 
	{
		Collection<List<String>> result = decoratedLearner.ComputeQuestions(pair, original, temp);
		Element questions = doc.createElement(ELEM_KINDS.ELEM_QUESTIONS.name());
		Element questionList = writeSequenceList(ELEM_KINDS.ATTR_QUESTIONS.name(), result);
		questions.appendChild(questionList);questions.appendChild(writePair(pair));
		topElement.appendChild(questions);topElement.appendChild(Transform.endl(doc));
		return result;
	}
	
	/** Stores the current learner input parameters. */
	public void writeLearnerEvaluationData(LearnerEvaluationConfiguration cnf)
	{
		topElement.appendChild(writeLearnerEvaluationConfiguration(cnf));topElement.appendChild(Transform.endl(doc));		
	}
	
	public LearnerGraph MergeAndDeterminize(LearnerGraph original, StatePair pair) 
	{
		LearnerGraph result = decoratedLearner.MergeAndDeterminize(original, pair);
		Element mergedGraph = series.writeGraph(result);
		Element mergeNode = doc.createElement(ELEM_KINDS.ELEM_MERGEANDDETERMINIZE.name());
		mergeNode.appendChild(mergedGraph);mergeNode.appendChild(writePair(new PairScore(pair.getQ(),pair.getR(),0,0)));
		topElement.appendChild(mergeNode);topElement.appendChild(Transform.endl(doc));
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

	public LearnerGraph init(final PTASequenceEngine engine, int plusSize, int minusSize) 
	{
		LearnerGraph result = decoratedLearner.init(engine,plusSize,minusSize);
		
		final PTASequenceEngine.FilterPredicate positiveFilter = engine.getFSM_filterPredicate(),
			negativeFilter = new PTASequenceEngine.FilterPredicate() {
			public boolean shouldBeReturned(Object name) {
				return !positiveFilter.shouldBeReturned(name);
			}
		};
		topElement.appendChild(writeInitialData(new InitialData(engine.getData(positiveFilter), plusSize, engine.getData(negativeFilter), minusSize, result)));
		return result;
	}

	public LearnerGraph init(Collection<List<String>> plus,	Collection<List<String>> minus) 
	{
		LearnerGraph result = decoratedLearner.init(plus, minus);
		
		topElement.appendChild(writeInitialData(new InitialData(plus, plus.size(), minus, minus.size(), result)));
		return result;
	}

	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
		Element restartElement = doc.createElement(ELEM_KINDS.ELEM_RESTART.name());
		restartElement.setAttribute(ELEM_KINDS.ATTR_KIND.name(),mode.toString());
		topElement.appendChild(restartElement);topElement.appendChild(Transform.endl(doc));
	}

	public void AugmentPTA(LearnerGraph pta, RestartLearningEnum ptaKind,
			List<String> sequence, boolean accepted, JUConstants newColour) 
	{
		decoratedLearner.AugmentPTA(pta, ptaKind, sequence, accepted, newColour);
		topElement.appendChild(writeAugmentPTA(new AugmentPTAData(ptaKind,sequence,accepted,newColour)));topElement.appendChild(Transform.endl(doc));
	}
}
