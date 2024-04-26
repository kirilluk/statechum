package statechum.analysis.learning.observers;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import junit_runners.ParameterizedWithName;
import junit_runners.ParameterizedWithName.ParametersToString;
import org.w3c.dom.Document;
import org.w3c.dom.Element;




import statechum.Configuration;
import statechum.StatechumXML;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import static statechum.TestHelper.checkForCorrectException;
import statechum.Configuration.LABELKIND;
import statechum.StatechumXML.LEGACY_StringSequenceWriter;
import statechum.StatechumXML.StringSequenceWriter;

public class TestRecordProgressDecorator 
{
	protected final Configuration config = Configuration.getDefaultConfiguration().copy();
	
	@Test
	public final void testWriteInputSequence1a() {
		List<String> inputSequence = Collections.emptyList();
		StringBuffer wr = new StringBuffer();new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals(LEGACY_StringSequenceWriter.seqStart+""+LEGACY_StringSequenceWriter.seqEnd,wr.toString());
	}
	
	@Test
	public final void testWriteInputSequence1b() {
		List<String> inputSequence = Collections.emptyList();
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence2a() {
		List<String> inputSequence = Collections.singletonList("this is a test");
		StringBuffer wr = new StringBuffer();new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals(LEGACY_StringSequenceWriter.seqStart+"this is a test"+LEGACY_StringSequenceWriter.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence2b() {
		List<String> inputSequence = Collections.singletonList("this is a test");
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"this is a test\"]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence3a() {
		List<String> inputSequence = Arrays.asList("this is a test","some more");
		StringBuffer wr = new StringBuffer();new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals(LEGACY_StringSequenceWriter.seqStart+"this is a test"+LEGACY_StringSequenceWriter.seqSep+"some more"+LEGACY_StringSequenceWriter.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence3b() {
		List<String> inputSequence = Arrays.asList("this is a test","some more");
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"this is a test\",\"some more\"]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence4a() {
		List<String> inputSequence = Arrays.asList("this is a test","some more","a");
		StringBuffer wr = new StringBuffer();new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals(LEGACY_StringSequenceWriter.seqStart+
				"this is a test"+LEGACY_StringSequenceWriter.seqSep+
				"some more"+LEGACY_StringSequenceWriter.seqSep+
				"a"+LEGACY_StringSequenceWriter.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence4b() {
		List<String> inputSequence = Arrays.asList("this is a test","some more","a");
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"this is a test\",\"some more\",\"a\"]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence_fail1()  {
		final List<String> inputSequence = Collections.singletonList("" + LEGACY_StringSequenceWriter.seqStart);
		final StringBuffer wr = new StringBuffer();
		checkForCorrectException(
				() -> new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence),IllegalArgumentException.class,"invalid characters");
	}

	@Test
	public final void testWriteInputSequence_F1()  {
		List<String> inputSequence = Collections.singletonList("" + LEGACY_StringSequenceWriter.seqStart);
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\""+LEGACY_StringSequenceWriter.seqStart+"\"]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence_fail2() {
		final List<String> inputSequence = Collections.singletonList("" + LEGACY_StringSequenceWriter.seqNewLine);
		final StringBuffer wr = new StringBuffer();
		checkForCorrectException(
				() -> new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence),IllegalArgumentException.class,"invalid characters");
	}

	@Test
	public final void testWriteInputSequence_F2() {
		List<String> inputSequence = Collections.singletonList("" + LEGACY_StringSequenceWriter.seqNewLine);
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"\\"+LEGACY_StringSequenceWriter.seqNewLine+"\"]",wr.toString());
	}

	@Test
	public final void testWriteInputSequence_fail3() {
		final List<String> inputSequence = Arrays.asList("this is a test","");
		final StringBuffer wr = new StringBuffer();
		checkForCorrectException(
				() -> new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence),IllegalArgumentException.class,"empty input in sequence");
	}
	
	@Test
	public final void testWriteInputSequence_F3() {
		List<String> inputSequence = Arrays.asList("this is a test","");
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"this is a test\",\"\"]",wr.toString());
	}
	
	@Test
	public final void testWriteInputSequence_fail4() {
		final List<String> inputSequence = Collections.singletonList("");
		final StringBuffer wr = new StringBuffer();
		checkForCorrectException(
				() -> new LEGACY_StringSequenceWriter(null).writeInputSequence(wr, inputSequence),IllegalArgumentException.class,"empty input in sequence");
	}
	
	@Test
	public final void testWriteInputSequence_F4() {
		List<String> inputSequence = Collections.singletonList("");
		StringBuffer wr = new StringBuffer();new StringSequenceWriter(null).writeInputSequence(wr, inputSequence);
		Assert.assertEquals("[\"\"]",wr.toString());
	}
	
	@Test
	public final void testReadInputSequence1() {
		List<String> expected = Collections.emptyList();
		List<String> actual = new LEGACY_StringSequenceWriter(null).readInputSequence(
				""+LEGACY_StringSequenceWriter.seqStart+LEGACY_StringSequenceWriter.seqEnd);
		Assert.assertEquals("wrong result: " + actual, expected, actual);
		
		actual = new StringSequenceWriter(null).readInputSequence(
				"[]");
		Assert.assertEquals("wrong result: " + actual, expected, actual);
	}

	@Test
	public final void testReadInputSequence2() {
		List<String> expected = Collections.singletonList("some more");
		List<String> actual = new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+"some more"+LEGACY_StringSequenceWriter.seqEnd);
		Assert.assertEquals("wrong result: " + actual + ", expected " + expected, expected, actual);
		
		actual = new StringSequenceWriter(null).readInputSequence(
				"[\"some more\"]");
		Assert.assertEquals("wrong result: " + actual + ", expected " + expected, expected, actual);
	}

	@Test
	public final void testReadInputSequence3() {
		List<String> expected = Arrays.asList("this is a test","some more");
		List<String> actual = new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+"this is a test"+LEGACY_StringSequenceWriter.seqSep+"some more"+LEGACY_StringSequenceWriter.seqEnd);
		Assert.assertEquals("wrong result: " + actual, expected, actual);
		
		actual = new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",\"some more\"]");
		Assert.assertEquals("wrong result: " + actual, expected, actual);
	}

	@Test
	public final void testReadInputSequence4() {
		List<String> expected = Arrays.asList("this is a test","some more","data");
		List<String> actual = new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+"this is a test"+
				LEGACY_StringSequenceWriter.seqSep+"some more"+
				LEGACY_StringSequenceWriter.seqSep+"data"+
				LEGACY_StringSequenceWriter.seqEnd);
		Assert.assertEquals("wrong result: " + actual, expected, actual);
		
		actual = new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\","+
				"\"some more\","+
				"\"data\""+
				"]");
		Assert.assertEquals("wrong result: " + actual, expected, actual);
	}
	
	@Test
	public final void testReadInputSequence5() {
		List<String> expected = Arrays.asList("this is a test","some more","data");
		List<String> actual = new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",\n"+
				"\"some more\","+
				"\"data\""+
				"]");
		Assert.assertEquals("wrong result: " + actual, expected, actual);
	}

	@Test
	public final void testReadInputSequence_fail2a1() {
		checkForCorrectException(() -> new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqEnd+"this is a test"+
				LEGACY_StringSequenceWriter.seqSep+"some more"+
				LEGACY_StringSequenceWriter.seqSep+"data"+
				LEGACY_StringSequenceWriter.seqEnd),IllegalArgumentException.class,"invalid char");
	}
	
	@Test
	public final void testReadInputSequence_fail2a2() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[this is a test,"+
				"some more,"+
				"data"+
				"]"),IllegalArgumentException.class,"invalid token");
	}

	@Test
	public final void testReadInputSequence_fail2b1() {
		checkForCorrectException(() -> new LEGACY_StringSequenceWriter(null).readInputSequence(LEGACY_StringSequenceWriter.seqEnd+
				LEGACY_StringSequenceWriter.seqStart+"this is a test"+
				LEGACY_StringSequenceWriter.seqSep+"some more"+
				LEGACY_StringSequenceWriter.seqSep+"data"+
				LEGACY_StringSequenceWriter.seqEnd),IllegalArgumentException.class,"invalid char");
	}

	@Test
	public final void testReadInputSequence_fail2b2() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence("]"+
				"[\"this is a test\","+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"invalid token");
	}

	@Test
	public final void testReadInputSequence_fail3a() {
		checkForCorrectException(() -> new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+"this is a test"+
				LEGACY_StringSequenceWriter.seqSep+"some more"+
				LEGACY_StringSequenceWriter.seqSep+"data"),IllegalArgumentException.class,"premature end");
	}

	@Test
	public final void testReadInputSequence_fail3b() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\","+
				"\"some more\","+
				"\"data\""),IllegalArgumentException.class,"unexpected end of list");
	}

	@Test
	public final void testReadInputSequence_fail4a() {
		checkForCorrectException(() -> new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+"this is a test"+
				LEGACY_StringSequenceWriter.seqSep+
				LEGACY_StringSequenceWriter.seqSep+"data"+
				LEGACY_StringSequenceWriter.seqEnd),IllegalArgumentException.class,"empty input");
	}

	@Test
	public final void testReadInputSequence_fail4b() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\","+
				"\"some more\",,"+
				"\"data\""+
				"]"),IllegalArgumentException.class,"unexpected comma");
	}

	@Test
	public final void testReadInputSequence_fail5a() {
		checkForCorrectException(() -> new LEGACY_StringSequenceWriter(null).readInputSequence(
				LEGACY_StringSequenceWriter.seqStart+
				LEGACY_StringSequenceWriter.seqSep+"some more"+
				LEGACY_StringSequenceWriter.seqSep+"data"+
				LEGACY_StringSequenceWriter.seqEnd),IllegalArgumentException.class,"invalid char");
	}

	@Test
	public final void testReadInputSequence_fail5b() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[,\"this is a test\","+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"unexpected comma");
	}

	/** Embedded list.*/
	@Test
	public final void testReadInputSequence_fail6() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",[],"+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"expected a string, got []");
	}

	/** Embedded tuple.*/
	@Test
	public final void testReadInputSequence_fail7() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",{},"+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"expected a string, got {}");
	}

	/** Embedded atom.*/
	@Test
	public final void testReadInputSequence_fail8() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",yy,"+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"expected a string, got yy");
	}

	/** Embedded number.*/
	@Test
	public final void testReadInputSequence_fail9() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[\"this is a test\",67,"+
				"\"some more\","+
				"\"data\""+
				"]"),IllegalArgumentException.class,"expected a string, got 67");
	}

	/** Embedded number.*/
	@Test
	public final void testReadInputSequence_fail10() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"{\"this is a test\","+
				"\"some more\","+
				"\"data\""+
				"}"),IllegalArgumentException.class,"expected a sequence, got {");
	}

	/** Embedded number.*/
	@Test
	public final void testReadInputSequence_fail11() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"56"),IllegalArgumentException.class,"expected a sequence, got 56");
	}

	/** More than one sequence.*/
	@Test
	public final void testReadInputSequence_fail12() {
		checkForCorrectException(() -> new StringSequenceWriter(null).readInputSequence(
				"[],[]"),IllegalArgumentException.class,"unexpected characters");
	}

	/** Used to remove tags from XML during testing. */
	public static final String junkTag = "JUNK";
	
	/** Given XML file in string, replaces a supplied tag with the 
	 * one with a different name, so as not to break the overall structure.
	 * 
	 * @param str what to modify
	 * @param tag what to replace
	 * @return modified XML
	 */
	public static String removeTagFromString(String str,String tag)
	{
		return str.replace(tag, junkTag);
	}
	
	/** Given XML file in string, replaces a supplied tag with the 
	 * one with a different name, so as not to break the overall structure.
	 * 
	 * @param str what to modify
	 * @param tag what to replace
	 * @return modified XML
	 */
	public static String removeTagFromString(String str,StatechumXML tag)
	{
		return removeTagFromString(str, tag.name());
	}
	
	/** Adds an extra attribute to the specified tag.
	 * 
	 * @param str XML to transform
	 * @param tag tag to modify
	 * @return modified XML
	 */
	public static String addExtraAttribute(String str,StatechumXML tag)
	{
		return str.replace("<"+tag.name(), "<"+tag.name()+" AA=\"45\" ");
	}
	
	/** Given XML file in string, replaces a value of the supplied tag with the 
	 * different value, so as not to break the overall structure but break the numerical value.
	 * 
	 * @param str what to modify
	 * @param tag what to replace
	 * @return modified XML
	 */
	public static String breakNumericalValue(String str, StatechumXML tag)
	{
		return str.replace(tag.name()+"=\"", tag.name()+"=\""+junkTag);
	}
	
	@SuppressWarnings("ConstantConditions")
	@RunWith(ParameterizedWithName.class)
	public static class TestSequenceDumping
	{
		@org.junit.runners.Parameterized.Parameters
		public static Collection<Object[]> data() 
		{
			List<Object[]> outcome = new LinkedList<>();
			for(boolean legacy:new boolean[]{true,false})
				for(LABELKIND lblKind:LABELKIND.values())
					outcome.add(new Object[]{legacy,lblKind});
			return outcome;
		}
		
		protected final Configuration config = Configuration.getDefaultConfiguration().copy();
		protected final ConvertALabel converter = null;
		protected Document doc = null;
		
		/** Creates the test class with the number of threads to create as an argument. */
		public TestSequenceDumping(Boolean legacyArg, LABELKIND labelKind)
		{
			config.setLabelKind(labelKind);config.setLegacyXML(legacyArg);
		}
		
		@ParametersToString
		public static String parametersToString(Boolean legacyArg, LABELKIND labelKind)
		{
			return (legacyArg ?"Legacy":"Current")+" "+labelKind;
		}
		
		@Test
		public final void testWriteSequences1() {
			List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			});
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			ProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			org.w3c.dom.Element dataElem = dumper.stringio.writeSequenceList("someData", data);
			
			List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}), actual = dumper.stringio.readSequenceList(dataElem,"someData");

			Assert.assertEquals("Expected " + expected + " got: " + actual, actual, expected);
		}
		
		@Test
		public final void testWriteSequences2() {
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().getBytes()),false,converter);
			loader.initIO(loader.doc, config);
			List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}), actual = loader.stringio.readSequenceList(loader.expectNextElement(StatechumXML.ELEM_SEQ.name()),"someData");
			Assert.assertEquals(actual, expected);
		}
	
		@Test
		public final void testWriteSequences3() {
			List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}),data2 = TestFSMAlgo.buildList(new String[][]{
					new String[]{},
					new String[]{},
					new String[]{},
					new String[]{"the second set of data"}
			});
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("someData", data));
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("moreData", data2));
			dumper.close();
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(output.toByteArray()),false,converter);
			loader.initIO(loader.doc, config);
			List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}), expected2 = TestFSMAlgo.buildList(new String[][]{
					new String[]{},
					new String[]{},
					new String[]{},
					new String[]{"the second set of data"}
			}),actual = loader.stringio.readSequenceList(loader.expectNextElement(StatechumXML.ELEM_SEQ.name()),"someData"),
			actual2 = loader.stringio.readSequenceList(loader.expectNextElement(StatechumXML.ELEM_SEQ.name()),"moreData");
			Assert.assertEquals(actual, expected);
			Assert.assertEquals(actual2, expected2);
		}
	
		/** Tests that during processing of an XML file I can step back. */
		@Test
		public final void testWriteSequences4() {
			List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}),data2 = TestFSMAlgo.buildList(new String[][]{
					new String[]{},
					new String[]{},
					new String[]{},
					new String[]{"the second set of data"}
			});
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("someData", data));
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("moreData", data2));
			dumper.close();
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(output.toByteArray()),false,converter);
			loader.initIO(loader.doc, config);
			List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}), expected2 = TestFSMAlgo.buildList(new String[][]{
					new String[]{},
					new String[]{},
					new String[]{},
					new String[]{"the second set of data"}
			});
			Element someDataElement = loader.expectNextElement(StatechumXML.ELEM_SEQ.name());
			List<List<String>> actual = loader.stringio.readSequenceList(someDataElement,"someData");
			Assert.assertEquals(actual, expected);
	
			loader.setNextElement(someDataElement);Assert.assertSame(someDataElement,loader.expectNextElement(StatechumXML.ELEM_SEQ.name()));
			loader.setNextElement(someDataElement);Assert.assertSame(someDataElement,loader.expectNextElement(StatechumXML.ELEM_SEQ.name()));
			// after a few attempts at setting the existing element back, we continue with the one to follow.
			
			Element moreDataElement = loader.expectNextElement(StatechumXML.ELEM_SEQ.name());
			List<List<String>> actual2 = loader.stringio.readSequenceList(moreDataElement,"moreData");
			Assert.assertEquals(actual2, expected2);
			
			// now we force the old element to be re-discovered
			loader.setNextElement(someDataElement);Assert.assertSame(someDataElement,loader.expectNextElement(StatechumXML.ELEM_SEQ.name()));
			actual = loader.stringio.readSequenceList(someDataElement,"someData");
			Assert.assertEquals(actual, expected);
		}
	
		/** Invalid XML file. */
		@Test(expected=IllegalArgumentException.class)
		public final void testWriteSequences_fail1() {
			LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceFirst(StatechumXML.ELEM_SEQ.name(), "TT").getBytes()),false,converter);
			List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			}), actual = loader.stringio.readSequenceList(loader.expectNextElement("whatever"),"someData");
			Assert.assertEquals(actual, expected);
		}
		
		/** Expected tag not found */
		@Test
		public final void testWriteSequences_fail2() {
			final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceAll(StatechumXML.ELEM_SEQ.name(), "TT").getBytes()),false,converter);
			checkForCorrectException(() -> loader.expectNextElement("U"),IllegalArgumentException.class,"encountered");
		}
		
		protected String dumpSequencesHelper()
		{
			List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			});
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("someData", data));
			dumper.close();
			return output.toString();
		}
		
		/** Wrong tag when doing readSequenceList */
		@Test
		public final void testWriteSequences_fail3() {
			final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceAll(StatechumXML.ELEM_SEQ.name(), "TT").getBytes()),false,converter);
			loader.initIO(loader.doc, config);
			checkForCorrectException(
					() -> loader.stringio.readSequenceList(loader.expectNextElement("TT"),"someData"),IllegalArgumentException.class,"expecting to load a list of sequences");
		}
	
		/** The name given to the collection of sequences does not match. */
		@Test
		public final void testWriteSequences_fail4() {
			List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
					new String[]{ "a","this is a test","3"},
					new String[]{},
					new String[]{},
					new String[]{"more data"}
			});
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			dumper.initIO(dumper.doc, config);
			dumper.topElement.appendChild(dumper.stringio.writeSequenceList("someData", data));
			dumper.close();
			
			final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(output.toByteArray()),false,converter);
			loader.initIO(loader.doc, config);
			checkForCorrectException(
					() -> loader.stringio.readSequenceList(loader.expectNextElement(StatechumXML.ELEM_SEQ.name()),"AsomeData"),IllegalArgumentException.class,"expecting to load a list with name ");
		}
	
		@Test
		public final void testCheckSingles1()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);

			Set<String> singlesCollection = new TreeSet<>(Arrays.asList("A", "B"));
			ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection);
		}
		
		@Test
		public final void testCheckSingles2()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			
			ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, null);
		}
		
		@Test
		public final void testCheckSingles_fail1()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);

			final Set<String> singlesCollection = new TreeSet<>(Arrays.asList("A", "B"));
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail2()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);

			final Set<String> singlesCollection = new TreeSet<>(Arrays.asList("A", "B"));
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail3()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);

			final Set<String> singlesCollection = new TreeSet<>(Arrays.asList("A", "B"));
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail4()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, null),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail5()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, null),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail6()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, null),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail7()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);

			final Set<String> singlesCollection = new TreeSet<>(Collections.singletonList("B"));
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection),IllegalArgumentException.class,"duplicate");
		}
		
		@Test
		public final void testCheckSingles_fail8()
		{
			ByteArrayOutputStream output = new ByteArrayOutputStream();
			final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,config,false);
			Element A;
			A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
			A=dumper.doc.createElement("C");dumper.topElement.appendChild(A);

			final Set<String> singlesCollection = new TreeSet<>(Arrays.asList("A", "B"));
			checkForCorrectException(
					() -> ProgressDecorator.checkChildrenAreUniquelyNamed(dumper.topElement, singlesCollection),IllegalArgumentException.class,"found unexpected elements");
		}
		
		@Test
		public final void testIntToString1()
		{
			Assert.assertEquals("34",RecordProgressDecorator.intToString(34, -1));
			Assert.assertEquals("34",RecordProgressDecorator.intToString(34, 1));
			Assert.assertEquals("34",RecordProgressDecorator.intToString(34, 2));
		}
	
		@Test
		public final void testIntToString2()
		{
			Assert.assertEquals("034",RecordProgressDecorator.intToString(34, 3));
			Assert.assertEquals("0034",RecordProgressDecorator.intToString(34, 4));
			Assert.assertEquals("00034",RecordProgressDecorator.intToString(34, 5));
		}
	}
}
