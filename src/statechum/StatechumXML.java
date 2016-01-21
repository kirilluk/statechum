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

package statechum;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public enum StatechumXML 
{
	/** GD XML tags. */
	gdGD("GD"), gdAdded("gdAdded"), gdRemoved("gdRemoved"), gdRelabelling("gdRelabelling"),

	/** Observer-related. */
	ELEM_ANSWER, ELEM_QUESTIONS, ATTR_QUESTIONS, ATTR_MOREQUESTIONS, ELEM_PAIRS, ELEM_STATECHUM_TESTTRACE, 
		ATTR_QUESTION, ATTR_TESTSET, ATTR_FAILEDPOS, ATTR_LTL, ELEM_PAIR, ELEM_SEQ, ATTR_SEQ, ATTR_Q, ATTR_R, ATTR_SCORE, ATTR_OTHERSCORE, ELEM_RESTART, ATTR_KIND, 
		ELEM_EVALUATIONDATA,ATTR_GRAPHKIND, ATTR_CONSTRAINTSADDED, ELEM_INIT, ELEM_MERGEANDDETERMINIZE, ATTR_LEARNINGOUTCOME, 
		ATTR_POSITIVE_SIZE, ATTR_POSITIVE_SEQUENCES, ATTR_NEGATIVE_SIZE, ATTR_NEGATIVE_SEQUENCES,
		ELEM_CONSTRAINTS,ELEM_AUGMENTPTA, ATTR_ACCEPT, ATTR_COLOUR, ELEM_PROGRESSINDICATOR, ELEM_LABELDETAILS, ATTR_GRAPHNUMBER, ATTR_WITHCONSTRAINTS,

	/** Graphml namespace */
	graphmlNS("gml"),
	
	/** Graphml top-level node tag if a name space is not used. */
	graphmlNodeName("graphml"),
	
	/** Graphml top-level node tag. */
	graphmlNodeNameNS(graphmlNS+":"+graphmlNodeName),
	
	/** Graphml uri */
	graphlmURI("http://graphml.graphdrawing.org/xmlns/graphml");	
	
	
	private final String stringRepresentation;
	
	StatechumXML(String textualName) {stringRepresentation = textualName;}
	StatechumXML() {stringRepresentation = null;}
	
	@Override
	public String toString()
	{
		if (stringRepresentation == null)
			return super.name();
		
		return stringRepresentation;
	}
	
	/** Given an element, this method returns all direct descendants with the specified tag. At most two elements will be returned.
	 */
	public static NodeList getChildWithTag(Element elem, String tag)
	{
		Node resultA = null, resultB = null; int counter=0;
		NodeList children = elem.getChildNodes();
		if (children.getLength() > 0)
			for(int i=0;i< children.getLength();++i)
				if (tag.equals(children.item(i).getNodeName()))
				{// found an element with an expected tag.
					if (resultA == null)
						resultA = children.item(i);
					else
						if (resultB == null)
							resultB = children.item(i);
						
					++counter;
				}
		
		final int finalCounter = counter;final Node finalNodeA = resultA, finalNodeB = resultB;
		
		return new NodeList() {

			@Override 
			public int getLength() {
				return finalCounter;
			}

			@Override 
			public Node item(int index) {
				if (index == 0)
					return finalNodeA;// the first element if available
				if (index == 1)
					return finalNodeB;// the second element if available
				throw new IllegalArgumentException("only "+finalCounter+" elements in a collectin but requested element "+index);
			}};
	}

	/** Writing sequences may require use of various tags for different kinds of sequences
	 * and elements may require conversion in order to be read or written.
	 */
	public static interface SequenceIO<ELEM_TYPE>
	{
		/** Given a collection of sequences, it writes them out in a form of XML element.
		 * 
		 * @param name the tag of the new element
		 * @param data what to write
		 * @return the written element.
		 */ 
		abstract public Element writeSequenceList(final String name, Collection<List<ELEM_TYPE>> data);
		
		
		/** Given an element, loads the data contained in it back into a collection.
		 * (this is an inverse of <em>addSequenceList</em>.
		 * 
		 * @param elem the element to load from
		 * @param expectedName the name which should have been given to this collection
		 * @return the collection of sequences of strings loaded from that element.
		 */
		abstract public List<List<ELEM_TYPE>> readSequenceList(Element elem, String expectedName);

		/** Dumps a sequence of inputs to the writer.
		 * 
		 * @param wr where to write sequences
		 * @param str sequence of inputs to write out. The collection can be empty but no input can be of length zero.
		 * @throws IOException
		 */	
		abstract public void writeInputSequence(StringBuffer wr,Collection<ELEM_TYPE> str);
		
		/** Loads a sequence of inputs from a string.
		 * It does not create an XML element because the outcome may often be stored in an attribute
		 * rather than inside an XML element.
		 * 
		 * @param string data to read
		 * @return collection of inputs read from a stream
		 */
		abstract public List<ELEM_TYPE> readInputSequence(String string);
	}
	
	public abstract static class XMLSequenceWriter<ELEM_TYPE> implements SequenceIO<ELEM_TYPE> 
	{			
		final protected Document doc;
		public XMLSequenceWriter(Document d)
		{
			doc = d;
		}
		
		/** Given a collection of sequences, it writes them out in a form of XML element.
		 * 
		 * @param name the tag of the new element
		 * @param data what to write
		 * @return the written element.
		 */ 
		@Override
		abstract public Element writeSequenceList(final String name, Collection<List<ELEM_TYPE>> data);
		
		
		/** Given a collection of sequences, it writes them out in a form of XML element.
		 * 
		 * @param name the tag of the new element
		 * @param data what to write
		 * @param separator what to separate sequences with
		 * @param prefix what to prefix a list with
		 * @param suffix what to end a list with
		 * @return the written element.
		 */ 
		public Element writeSequenceList(final String name, Collection<List<ELEM_TYPE>> data,String separator,String prefix, String suffix)
		{
			Element sequenceListElement = doc.createElement(StatechumXML.ELEM_SEQ.name());
			sequenceListElement.setAttribute(StatechumXML.ATTR_SEQ.name(), name.toString());
			StringBuffer strBuffer = new StringBuffer();if (prefix != null) strBuffer.append(prefix);
			boolean first = true;
			for(List<ELEM_TYPE> seq:data)
			{
				if (!first) strBuffer.append(separator);else first = false;
				writeInputSequence(strBuffer, seq);
			}
			if (suffix != null) strBuffer.append(suffix);
			org.w3c.dom.Text dataInNode = doc.createTextNode(strBuffer.toString());// if the string is empty at this point, the text node will not get added, so I have to check that there is any at the loading stage.
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
		@Override
		public List<List<ELEM_TYPE>> readSequenceList(Element elem, String expectedName)
		{
			List<List<ELEM_TYPE>> result = new LinkedList<List<ELEM_TYPE>>();
			if (!elem.getNodeName().equals(StatechumXML.ELEM_SEQ.name()))
				throw new IllegalArgumentException("expecting to load a list of sequences "+elem.getNodeName());
			if (!elem.getAttribute(StatechumXML.ATTR_SEQ.name()).equals(expectedName))
				throw new IllegalArgumentException("expecting to load a list with name "+expectedName+
						" but found a list named "+elem.getAttribute(StatechumXML.ATTR_SEQ.name()));
			if (elem.getFirstChild() != null)
				result.addAll(readSequenceList(elem.getFirstChild().getTextContent()));
			return result;
		}
		
		/** Loads a sequence of sequences from the supplied trace into a collection.
		 * 
		 * @param elem the element to load from
		 * @return the collection of sequences of strings loaded from that element.
		 */
		abstract public List<List<ELEM_TYPE>> readSequenceList(String sequence);
		
		/** Dumps a sequence of inputs to the buffer.
		 * 
		 * @param wr where to write sequences
		 * @param str sequence of inputs to write out. The collection can be empty but no input can be of length zero.
		 * @throws IOException
		 */	
		@Override
		abstract public void writeInputSequence(StringBuffer wr,Collection<ELEM_TYPE> str);
	}
	
	public static class StringSequenceWriter extends XMLSequenceWriter<String>
	{

		public StringSequenceWriter(Document d) {
			super(d);
		}

		/** Loads a sequence of the form [elem1,elem2, ... ] */
		@Override
		public List<String> readInputSequence(String string) {
			OtpErlangObject obj = ErlangLabel.parseText(string);
			return readSequenceFromErlangObject(obj);
		}

		public List<String> readSequenceFromErlangObject(OtpErlangObject obj) 
		{
			if (!(obj instanceof OtpErlangList))
				throw new IllegalArgumentException("expected a sequence, got "+obj);
			List<String> outcome = new LinkedList<String>();
			for(OtpErlangObject o:((OtpErlangList)obj))
			{
				if (!(o instanceof OtpErlangString))
					throw new IllegalArgumentException("expected a string, got "+o);
				outcome.add(((OtpErlangString)o).stringValue());
			}
			return outcome;
		}
		
		/** Loads a collection of sequences of the form [ [elem1,elem2, ... ], [elem11,elem12, ... ] ] */
		@Override
		public List<List<String>> readSequenceList(String sequence) 
		{
			OtpErlangObject obj = ErlangLabel.parseText(sequence);
			if (!(obj instanceof OtpErlangList))
				throw new IllegalArgumentException("expected a sequence of sequences, got "+obj);
			List<List<String>> outcome = new LinkedList<List<String>>();
			for(OtpErlangObject o:((OtpErlangList)obj))
				outcome.add(readSequenceFromErlangObject(o));
			return outcome;
		}

		/** Dumps the supplied collection of strings into the supplied buffer. */
		@Override
		public void writeInputSequence(StringBuffer wr, Collection<String> str) 
		{
			boolean first = true;wr.append('[');
			for(String st:str)
			{
				if (!first) wr.append(',');else first = false;
				ErlangLabel.ErlangString.getSingleton().dump(st, wr);
			}
			wr.append(']');
		}

		@Override
		public Element writeSequenceList(String name, Collection<List<String>> data) 
		{
			return writeSequenceList(name, data, ",\n","[", "]");
		}
		
	}

	/** An implementation of this interface for sequences of strings. */
	public static class LabelSequenceWriter extends XMLSequenceWriter<Label>
	{
		private final Configuration config;
		private final ConvertALabel converter;
		
		public LabelSequenceWriter(Document d, Configuration conf, ConvertALabel conv) {
			super(d);config = conf;converter = conv;
		}

		@Override
		public List<Label> readInputSequence(String string) 
		{
			return statechum.StatechumXML.readInputSequence(string,config,converter);
		}

		@Override
		public List<List<Label>> readSequenceList(String sequence)
		{
			return StatechumXML.readSequenceList(sequence,config,converter);
		}
		
		@Override
		public void writeInputSequence(StringBuffer wr, Collection<Label> str) {
			wr.append('[');
			boolean first = true;
			for(Label label:str)
			{
				if (!first) wr.append(',');else first = false;
				if (label instanceof ErlangLabel)
					wr.append(label.toErlangTerm());
				else if (label instanceof StringLabel)
					ErlangLabel.ErlangQuotedAtom.getSingleton().dump(label.toErlangTerm(), wr);
				else
					throw new IllegalArgumentException("cannot store label of type "+label.getClass());
			}
			wr.append(']');
		}

		@Override
		public Element writeSequenceList(String name,Collection<List<Label>> data) {
			return writeSequenceList(name, data, ",\n","[", "]");
		}
	
	}
	
	public static List<List<Label>> readSequenceList(String sequence,Configuration config, ConvertALabel conv) 
	{
    	return readSequenceList(ErlangLabel.parseText(sequence),config,conv);
	}
    
	public static List<List<Label>> readSequenceList(OtpErlangObject obj,Configuration config, ConvertALabel conv) 
	{
		if (!(obj instanceof OtpErlangList))
			throw new IllegalArgumentException("expected a sequence, got "+obj);
    	OtpErlangList list = (OtpErlangList)obj;
    	List<List<Label>> outcome = new LinkedList<List<Label>>();
    	for(OtpErlangObject o:list.elements())
    		outcome.add(readInputSequenceFromErlangObject(o,config,conv));
    	return outcome;
	}

	public static List<Label> readInputSequence(String string, Configuration config, ConvertALabel conv) 
	{
    	OtpErlangObject obj = ErlangLabel.parseText(string);
    	return readInputSequenceFromErlangObject(obj,config,conv);
	}

	/** Given a string containing the whole of the expression to parse, parses the text and returns the
     * corresponding Erlang trace. This is not a complete trace - it cannot really be passed to Erlang 
     * for execution because labels of which it consists are not associated with actual functions.
     *  
     * @param str label to parse
     * @param config configuration to use 
     * Uses the module in the configuration to provide definitions of functions to associated with labels. Will 
     * generic name <em>ErlangLabel.missing</em> if <em>mod</em> is null.
     * @param conv converter for labels, not used if null.
     * @return the outcome.
     */
	public static List<Label> readInputSequenceFromErlangObject(OtpErlangObject obj,Configuration config, ConvertALabel conv) 
	{
		if (!(obj instanceof OtpErlangList))
			throw new IllegalArgumentException("expected a sequence, got "+obj);
    	OtpErlangList list = (OtpErlangList)obj;
    	List<Label> outcome = new LinkedList<Label>();
    	
    	for(OtpErlangObject o:list.elements())
    	{
    		Label label = null;
    		switch(config.getLabelKind())
    		{
    		case LABEL_STRING:
    			if (!(o instanceof OtpErlangAtom))
    				throw new IllegalArgumentException("a string label has to look like Erlang Atom");
    			label = new StringLabel( ((OtpErlangAtom)o).atomValue() );
    			break;
    		case LABEL_ERLANG:
    			label = ErlangLabel.erlangObjectToLabel(o,config);
    			break;
    		default:
    			throw new IllegalArgumentException("No parser available for traces of type "+config.getLabelKind());
    		}
    		
    		if (conv != null) label = conv.convertLabelToLabel(label);
    		outcome.add(label);
    		
    	}
    	return outcome;
	}

	/** An implementation of this interface for sequences of strings. */
	public static class LEGACY_StringSequenceWriter extends XMLSequenceWriter<String>
	{
		
		public LEGACY_StringSequenceWriter(Document d) {
			super(d);
		}

		private final static Pattern patternBadChars;
		
		/** A mini-parser is used in conjunction with XML to reduce the size of test trace
		 * files - using single chars helps reduce test trace files greatly, making sure
		 * both my slow uplink and SF are happy.
		 */
		public static final char seqStart='{',seqEnd='}',seqSep=',',seqNewLine='\n';
		
		private static final String badChars = "\\"+seqStart+"\\"+seqSep+"\\"+seqEnd+seqNewLine;
		

		static
		{
			patternBadChars = Pattern.compile("["+badChars+"]");
		}

		/** Loads a sequence of sequences from the supplied trace into a collection.
		 * 
		 * @param elem the element to load from
		 * @return the collection of sequences of strings loaded from that element.
		 */
		@Override
		public List<List<String>> readSequenceList(String sequence)
		{
			List<List<String>> result = new LinkedList<List<String>>();
			Reader reader = new StringReader(sequence);
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
				statechum.Helper.throwUnchecked("failed to write to writer ",e);
			}
			
			return result;
		}
		
		/** Dumps a sequence of inputs to the writer.
		 * 
		 * @param wr where to write sequences
		 * @param str sequence of inputs to write out. The collection can be empty but no input can be of length zero.
		 * @throws IOException
		 */	
		@Override
		public void writeInputSequence(StringBuffer wr,Collection<String> str)
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
		
		/** Loads a sequence of inputs from a reader. Since I do not wish to use <em>mark</em>
		 * or some form of put-back but would like a way to see ahead, I decided simply to pass
		 * the first character as a parameter, -1 if there is none.
		 * 
		 * @param rd stream to read
		 * @param firstChar the first character, -1 if the first char is to be read from a stream
		 * @return collection of inputs read from a stream
		 */
		@Override
		public List<String> readInputSequence(String data)
		{
			return readInputSequence(new StringReader(data),-1);
		}

		public List<String> readInputSequence(Reader rd, int firstChar)
		{
			List<String> result = new LinkedList<String>();
			try
			{
				int ch = (firstChar == -1?rd.read():firstChar);while(ch == seqNewLine) ch=rd.read();if (ch != seqStart) throw new IllegalArgumentException("invalid char "+ch+" instead of a sequence");
				boolean after_open_bracket = true;
				do
				{
					StringBuffer input = new StringBuffer();
					ch = rd.read();
					while(ch != -1 && ch == ' ')
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
				statechum.Helper.throwUnchecked("failed to read from reader ",e);
			}
			return result;
		}

		@Override
		public Element writeSequenceList(String name, Collection<List<String>> data) 
		{
			return writeSequenceList(name, data, "\n", null, null);
		}
	}
	
	/** An implementation of this interface for sequences of string labels - this is a compatibility 
	 * interface aimed at migration from old to new logs. Delegates most of what it does to 
	 * the original writer. The conversion process is extremely inefficient - there is no reason
	 * for it to be better since the aim is to obsolete this after the conversion is complete.
	 */
	public static class LEGACY_StringLabelSequenceWriter implements SequenceIO<Label>
	{
		final protected LEGACY_StringSequenceWriter delegate;
		final protected Configuration config;
		final protected ConvertALabel converter;
		
		public LEGACY_StringLabelSequenceWriter(Document d,Configuration conf, ConvertALabel conv)
		{
			delegate = new LEGACY_StringSequenceWriter(d);config = conf;converter = conv;
		}
		
		@Override
		public Element writeSequenceList(String name, Collection<List<Label>> data) 
		{
			List<List<String>> dataToWrite = new ArrayList<List<String>>(data.size());
			for(List<Label> seq:data)
			{
				List<String> s = new ArrayList<String>(seq.size());
				for(Label l:seq) s.add(l.toErlangTerm());
				dataToWrite.add(s);
			}
			return delegate.writeSequenceList(name, dataToWrite);
		}

		@Override
		public List<List<Label>> readSequenceList(Element elem, String expectedName) 
		{
			List<List<String>> data = delegate.readSequenceList(elem, expectedName);
			List<List<Label>> convertedData = new ArrayList<List<Label>>(data.size());
			for(List<String> str:data)
				convertedData.add(AbstractLearnerGraph.buildList(str, config, converter));

			return convertedData;
		}

		@Override
		public void writeInputSequence(StringBuffer wr, Collection<Label> str) {
			List<String> s = new ArrayList<String>(str.size());
			for(Label l:str) s.add(l.toErlangTerm());
			delegate.writeInputSequence(wr, s);
		}

		@Override
		public List<Label> readInputSequence(String data) {
			return AbstractLearnerGraph.buildList(delegate.readInputSequence(data),config, converter);
		}
		
	}
}
