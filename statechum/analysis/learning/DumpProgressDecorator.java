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

/* Design problems:
 * 
 * XMLEncoder is a good idea in theory - it records a sequence of Java commands 
 * which would be necessary to set the properties of Java beans to load the data 
 * (which has yet to be stored). This mechanism even supports non-default 
 * constructors and factory methods. The trouble is that it is very verbose - 
 * 500Meg of a trace of Statechum is too much for CVS version control over 256k 
 * uplink, likely even for SF to store. GZIPpping kind of defeats the purpose, 
 * because I'd like CVS to do diff and be able to look at it on the screen
 * when tests fail some day (with having to check out the version when they
 * still worked, or install a different JVM or whatever). 
 * For this reason, I have to find ways for compate serialisation/deserialisation 
 * of all such data. Simply using a comma-separated format seems best 
 * for the purpose of storing tests - I do feel like XML being too verbose, 
 * but sequences are not hierarchical hence no need to worry about schema 
 * in need of subclassing and such. For this reason, graphs are stored as GraphML
 * and the rest in some XML interspersed with comma-separated sequences.
 * The intension for the graphs to be stored in the graph-difference format.
 */

package statechum.analysis.learning;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.regex.Pattern;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import statechum.JUConstants;
import statechum.analysis.learning.computeStateScores.PairScore;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public class DumpProgressDecorator extends LearnerDecorator {

	private final static Pattern patternBadChars;
	
	/** A mini-parser is used in conjunction with XML to reduce the size of test trace
	 * files - using single chars helps reduce testtrace files greately, making sure
	 * both my slow uplink and SF are happy.
	 */
	public static final char seqStart='[',seqEnd=']',seqSep=',',seqNewLine='\n';
	
	protected Document doc = null;
	Element topElement = null;
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
			topElement = doc.createElement(ELEM_KINDS.ELEM_STATECHUM_TESTTRACE.toString());doc.appendChild(topElement);
		}
		catch(ParserConfigurationException e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to construct DOM document");ex.initCause(e);
			throw ex;
		}
	}

	public DumpProgressDecorator(Learner learner, Reader inputReader) 
	{
		super(learner);outputWriter = null;

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try
		{
			factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);factory.setXIncludeAware(false);
			factory.setExpandEntityReferences(false);factory.setValidating(false);// we do not have a schema to validate against-this does not seem necessary for the simple data format we are considering here.
			doc = factory.newDocumentBuilder().parse(new org.xml.sax.InputSource(inputReader));
			topElement = doc.getDocumentElement();topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		}
		catch(Exception e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to construct/load DOM document");ex.initCause(e);
			throw ex;
		}
	}

	public void close()
	{
		try {
			Transformer trans = TransformerFactory.newInstance().newTransformer();
			trans.transform(new DOMSource(doc),new StreamResult(outputWriter));
		} catch (Exception e) {
			IllegalArgumentException ex = new IllegalArgumentException("failed to write out XML "+e);ex.initCause(e);
			throw ex;
		}		
	}
	
	static
	{
		patternBadChars = Pattern.compile("["+"\\"+seqStart+"\\"+seqSep+"\\"+seqEnd+seqNewLine+"]");
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

	public static enum ELEM_KINDS { ELEM_ANSWER, ELEM_QUESTIONS, ELEM_PAIRS, ELEM_PTA, ELEM_STATECHUM_TESTTRACE, 
		ATTR_QUESTION, ELEM_TESTSET, ATTR_FAILEDPOS, ATTR_LTL, ELEM_PAIR, ELEM_SEQ, ATTR_Q, ATTR_R, ATTR_SCORE, ELEM_RESTART, ATTR_KIND };
	
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
		addSequenceList(ELEM_KINDS.ELEM_QUESTIONS.toString(), result);
		return result;
	}
	
	/** Given a collection of sequences, it writes them out in a form of XML element.
	 * 
	 * @param name the tag of the new element
	 * @param data what to write
	 * @return the written element (which has already been added to the document.
	 */ 
	public Element addSequenceList(String name, Collection<List<String>> data)
	{
		Element sequenceListElement = doc.createElement(name);
		for(List<String> seq:data)
		{
			StringWriter strWriter = new StringWriter();writeInputSequence(strWriter,seq);
			Element sequenceElement = doc.createElement(ELEM_KINDS.ELEM_SEQ.toString());
			org.w3c.dom.Text dataInNode = doc.createTextNode(strWriter.toString());
			sequenceElement.appendChild(dataInNode);
			sequenceListElement.appendChild(sequenceElement);sequenceListElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		}
		topElement.appendChild(sequenceListElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
		return sequenceListElement;
	}
	
	/** Given an element, loads the data contained in it back into a collection.
	 * (this is an inverse of <em>addSequenceList</em>.
	 * 
	 * @param elem the element to load from
	 * @return the collection of sequences of strings loaded from that element.
	 */
	protected List<List<String>> readSequenceList(Element elem)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		NodeList children = elem.getChildNodes(); 
		for(int i=0;i<children.getLength();++i)
		{
			org.w3c.dom.Node node = children.item(i);
			if (!node.getNodeName().equals(ELEM_KINDS.ELEM_SEQ.toString()))
				throw new IllegalArgumentException("a list of sequences contains non-sequence "+node.getNodeName());
			result.add(readInputSequence(new StringReader(node.getFirstChild().getTextContent())));
		}
		return result;
	}
	
	public computeStateScores MergeAndDeterminize(computeStateScores original,
			StatePair pair) 
	{
		computeStateScores result = decoratedLearner.MergeAndDeterminize(original, pair);
		//encoder.writeObject(FSMSTructure_PersistenceDelegate.getGraphAsXML(WMethod.getGraphData(result.getGraph())));//result.getGraph());
		return result;
	}

	public String getResult() 
	{
		String result = decoratedLearner.getResult();
		return result;
	}

	public DirectedSparseGraph init(Collection<List<String>> plus,
			Collection<List<String>> minus) 
	{
		DirectedSparseGraph result = decoratedLearner.init(plus, minus);
		return result;
	}

	public void Restart(RestartLearningEnum mode) 
	{
		decoratedLearner.Restart(mode);
		Element restartElement = doc.createElement(ELEM_KINDS.ELEM_RESTART.toString());
		restartElement.setAttribute(ELEM_KINDS.ATTR_KIND.toString(),mode.toString());
		topElement.appendChild(restartElement);topElement.appendChild(TestFSMAlgo.FSMStructure.endl(doc));
	}
		
	/** Dumps a sequence of inputs to the writer.
	 * 
	 * @param wr where to write sequences
	 * @param str sequence of inputs to write out. The collection can be empty but no input can be of length zero.
	 * @throws IOException
	 */	
	public static void writeInputSequence(Writer wr,List<String> str)
	{
		try
		{
			wr.append(seqStart);
			boolean firstElem = true;
			for(String st:str)
			{
				if (!firstElem)	wr.append(seqSep);else firstElem = false;
				if (st.length() == 0)
					throw new IllegalArgumentException("empty input in sequence");
				if (patternBadChars.matcher(st).find())
					throw new IllegalArgumentException("invalid characters in sequence "+str+" : it matches "+patternBadChars.toString());
				wr.append(st);
			}
			wr.append(seqEnd);
		}
		catch(IOException e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to write to writer "+e);ex.initCause(e);
			throw ex;
		}
	}
	
	public static List<String> readInputSequence(Reader rd)
	{
		List<String> result = new LinkedList<String>();
		try
		{
			int ch = rd.read();while(ch == seqNewLine) ch=rd.read();if (ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
			boolean after_open_bracket = true;
			do
			{
				StringBuffer input = new StringBuffer();
				ch = rd.read();
				while(ch != -1 && ch != seqEnd && ch != seqSep)
				{
					input.append((char)ch);
					ch = rd.read();
				}
				if (ch == -1) throw new IllegalArgumentException("premature end of stream");
				if (!after_open_bracket && input.length() == 0)
					throw new IllegalArgumentException("empty input in a sequence of inputs");
				after_open_bracket = false;
				if (input.length() > 0)
					result.add(input.toString());// if length is zero and we did not throw an exception, this means that the stream is empty.
			}		
			while(ch != seqEnd);
		}
		catch(IOException e)
		{
			IllegalArgumentException ex = new IllegalArgumentException("failed to read from reader "+e);ex.initCause(e);
			throw ex;
		}
		return result;
	}
	
}
