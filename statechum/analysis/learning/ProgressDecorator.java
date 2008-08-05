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
 * The best thing about XMLEncoder is that it is not easy to integrate it into
 * convential XML processing by getting it to write to a node so that I could
 * then include this node in a document - the output of XMLEncoder will have to
 * be turned into a string and parsed by DOM parser. Loading will also involve
 * strings - seems like a crazy thing to do. 
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
import java.util.regex.Pattern;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import statechum.analysis.learning.TestFSMAlgo.FSMStructure;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

public abstract class ProgressDecorator extends LearnerDecorator 
{
	protected ProgressDecorator(Learner l)
	{
		super(l);
	}
	
	private final static Pattern patternBadChars;
	
	/** A mini-parser is used in conjunction with XML to reduce the size of test trace
	 * files - using single chars helps reduce testtrace files greately, making sure
	 * both my slow uplink and SF are happy.
	 */
	public static final char seqStart='[',seqEnd=']',seqSep=',',seqNewLine='\n';
	
	protected Document doc = null;
	Element topElement = null;
	
	/** Used extensively to convert checked exceptions to IllegalArgumentException
	 * (obviously in places where no exceptions should occur hence no need to use
	 * checked exceptions).
	 * 
	 * @param description description of why the exception is to be thrown
	 * @param e exception to convert
	 */
	public static void throwUnchecked(String description, Exception e)
	{
		IllegalArgumentException ex = new IllegalArgumentException(description+": "+e.getMessage());ex.initCause(e);
		throw ex;
	}
			
	static
	{
		patternBadChars = Pattern.compile("["+"\\"+seqStart+"\\"+seqSep+"\\"+seqEnd+seqNewLine+"]");
	}
	
	/** Stores the current learner input parameters. */
	abstract public void handleLearnerEvaluationData(FSMStructure fsm, Collection<List<String>> testSet);
	
	/** Dummy implementation, overridden in DumpProcessDecorator. */
	public void close()
	{
	}
	
	@Override
	public DirectedSparseGraph learnMachine(@SuppressWarnings("unused")	Learner top_level_decorator)
	{
		return decoratedLearner.learnMachine(this);
	}
	
	public static enum ELEM_KINDS { ELEM_ANSWER, ELEM_QUESTIONS, ELEM_PAIRS, ELEM_PTA, ELEM_STATECHUM_TESTTRACE, 
		ATTR_QUESTION, ELEM_TESTSET, ATTR_FAILEDPOS, ATTR_LTL, ELEM_PAIR, ELEM_SEQ, ATTR_SEQ, ATTR_Q, ATTR_R, ATTR_SCORE, ELEM_RESTART, ATTR_KIND, 
		ELEM_EVALUATIONDATA
	};
	
	/** Given a collection of sequences, it writes them out in a form of XML element.
	 * 
	 * @param name the tag of the new element
	 * @param data what to write
	 * @return the written element.
	 */ 
	protected Element addSequenceList(String name, Collection<List<String>> data)
	{
		Element sequenceListElement = doc.createElement(ELEM_KINDS.ELEM_SEQ.toString());
		sequenceListElement.setAttribute(ELEM_KINDS.ATTR_SEQ.toString(), name);
		StringWriter strWriter = new StringWriter();
		for(List<String> seq:data)
		{
			writeInputSequence(strWriter, seq);strWriter.append('\n');
		}
		org.w3c.dom.Text dataInNode = doc.createTextNode(strWriter.toString());// if the string is empty at this point, the text node will not get added, so I have to check that there is any at the loading stage.
		sequenceListElement.appendChild(dataInNode);
		return sequenceListElement;
	}
	
	/** Given an element, loads the data contained in it back into a collection.
	 * (this is an inverse of <em>addSequenceList</em>.
	 * 
	 * @param elem the element to load from
	 * @param expectedName the name which should have been given to this collection
	 * @return the collection of sequences of strings loaded from that element.
	 */
	protected List<List<String>> readSequenceList(Element elem, String expectedName)
	{
		List<List<String>> result = new LinkedList<List<String>>();
		if (!elem.getNodeName().equals(ELEM_KINDS.ELEM_SEQ.toString()))
			throw new IllegalArgumentException("expecting to load a list of sequences "+elem.getNodeName());
		if (!elem.getAttribute(ELEM_KINDS.ATTR_SEQ.toString()).equals(expectedName))
			throw new IllegalArgumentException("expecting to load a list with name "+expectedName+
					" but found a list named "+elem.getAttribute(ELEM_KINDS.ATTR_SEQ.toString()));
		if (elem.getFirstChild() != null)
		{
			Reader reader = new StringReader(elem.getFirstChild().getTextContent());
			try
			{
				int ch = reader.read();while(ch == seqNewLine) ch=reader.read();
				if (ch != -1 && ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
				
				while(ch == seqStart)
				{
					result.add(readInputSequence(reader,ch));
					ch = reader.read();while(ch == seqNewLine) ch=reader.read();
				}
				if (ch != -1 && ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
			}
			catch(IOException e)
			{
				throwUnchecked("failed to write to writer ",e);
			}
		}
		return result;
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
			throwUnchecked("failed to write to writer ",e);
		}
	}
	
	/** Loads a sequence of inputs from a reader. Since I do not wish to use <em>mark</em>
	 * or some form of put-back but would like a way to see ahead, I decided simply to pass
	 * the first character as a parameter, -1 if there is none.
	 * 
	 * @param rd stream to read
	 * @param firstChar the first character, -1 if the first char is to be read from a stream
	 * @return collection of inputs read from a stream
	 */
	public static List<String> readInputSequence(Reader rd, int firstChar)
	{
		List<String> result = new LinkedList<String>();
		try
		{
			int ch = firstChar == -1?rd.read():firstChar;while(ch == seqNewLine) ch=rd.read();if (ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
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
			throwUnchecked("failed to read from reader ",e);
		}
		return result;
	}
	
}
