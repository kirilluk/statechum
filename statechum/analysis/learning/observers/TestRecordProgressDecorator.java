package statechum.analysis.learning.observers;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Element;

import edu.uci.ics.jung.graph.impl.DirectedSparseGraph;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.AbstractOracle;
import statechum.analysis.learning.RPNIBlueFringeLearner;
import statechum.analysis.learning.observers.ProgressDecorator.ELEM_KINDS;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import static statechum.Helper.whatToRun;
import static statechum.Helper.checkForCorrectException;
import static statechum.analysis.learning.rpnicore.TestFSMAlgo.buildSet;

public class TestRecordProgressDecorator {

	@Test
	public final void testWriteInputSequence1() {
		List<String> inputSequence = Arrays.asList(new String[] {});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+""+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence2() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test"});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+"this is a test"+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence3() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test","some more"});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+"this is a test"+ProgressDecorator.seqSep+"some more"+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence4() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test","some more","a"});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+
				"this is a test"+ProgressDecorator.seqSep+
				"some more"+ProgressDecorator.seqSep+
				"a"+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail1()  {
		List<String> inputSequence = Arrays.asList(new String[] {""+ProgressDecorator.seqStart});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+""+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail2() {
		List<String> inputSequence = Arrays.asList(new String[] {""+ProgressDecorator.seqNewLine});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+""+ProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail3() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test",""});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+""+ProgressDecorator.seqEnd,wr.toString());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail4() {
		List<String> inputSequence = Arrays.asList(new String[] {""});
		Writer wr = new StringWriter();ProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(ProgressDecorator.seqStart+""+ProgressDecorator.seqEnd,wr.toString());
	}
	
	@Test
	public final void testReadInputSequence1() {
		List<String> expected = Arrays.asList(new String[] {});
		List<String> actual = ProgressDecorator.readInputSequence(new StringReader(
				""+ProgressDecorator.seqStart+ProgressDecorator.seqEnd),-1);
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence2() {
		List<String> expected = Arrays.asList(new String[] {"some more"});
		List<String> actual = ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"some more"+ProgressDecorator.seqEnd),-1);
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence3() {
		List<String> expected = Arrays.asList(new String[] {"this is a test","some more"});
		List<String> actual = ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"this is a test"+ProgressDecorator.seqSep+"some more"+ProgressDecorator.seqEnd),-1);
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence4() {
		List<String> expected = Arrays.asList(new String[] {"this is a test","some more","data"});
		List<String> actual = ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"this is a test"+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),-1);
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}
	
	@Test
	public final void testReadInputSequence5() {
		List<String> expected = Arrays.asList(new String[] {"this is a test","some more","data"});
		List<String> actual = ProgressDecorator.readInputSequence(new StringReader(
				"this is a test"+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),ProgressDecorator.seqStart);
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail2a() {
		ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqEnd+"this is a test"+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),-1);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail2b() {
		ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"this is a test"+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),ProgressDecorator.seqEnd);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail3() {
		ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"this is a test"+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"),-1);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail4() {
		ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+"this is a test"+
				ProgressDecorator.seqSep+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),-1);
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail5() {
		ProgressDecorator.readInputSequence(new StringReader(
				ProgressDecorator.seqStart+
				ProgressDecorator.seqSep+"some more"+
				ProgressDecorator.seqSep+"data"+
				ProgressDecorator.seqEnd),-1);
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
		ProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		org.w3c.dom.Element dataElem = dumper.writeSequenceList("someData", data);
		
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = ProgressDecorator.readSequenceList(dataElem,"someData");
		
		Assert.assertTrue(actual.equals(expected));
	}
	
	@Test
	public final void testWriteSequences2() {
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().getBytes()),false);
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = ProgressDecorator.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.name()),"someData");
		Assert.assertTrue(actual.equals(expected));
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
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(dumper.writeSequenceList("someData", data));
		dumper.topElement.appendChild(dumper.writeSequenceList("moreData", data2));
		dumper.close();
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(output.toByteArray()),false);
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
		}),actual = ProgressDecorator.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.name()),"someData"),
		actual2 = ProgressDecorator.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.name()),"moreData");
		Assert.assertTrue(actual.equals(expected));
		Assert.assertTrue(actual2.equals(expected2));
	}

	/** Invalid XML file. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteSequences_fail1() {
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceFirst(ELEM_KINDS.ELEM_SEQ.name(), "TT").getBytes()),false);
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = ProgressDecorator.readSequenceList(loader.expectNextElement("whatever"),"someData");
		Assert.assertTrue(actual.equals(expected));
	}
	
	/** Expected tag not found */
	@Test
	public final void testWriteSequences_fail2() {
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceAll(ELEM_KINDS.ELEM_SEQ.name(), "TT").getBytes()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			loader.expectNextElement("U");
		}},IllegalArgumentException.class,"encountered");
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
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(dumper.writeSequenceList("someData", data));
		dumper.close();
		return output.toString();
	}
	
	/** Wrong tag when doing readSequenceList */
	@Test
	public final void testWriteSequences_fail3() {
		checkForCorrectException(new whatToRun() { public void run() {
		LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(dumpSequencesHelper().replaceAll(ELEM_KINDS.ELEM_SEQ.name(), "TT").getBytes()),false);
			ProgressDecorator.readSequenceList(loader.expectNextElement("TT"),"someData");
		}},IllegalArgumentException.class,"expecting to load a list of sequences");
	}

	/** The name given to the collection of sequences does not match. */
	@Test
	public final void testWriteSequences4() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		dumper.topElement.appendChild(dumper.writeSequenceList("someData", data));
		dumper.close();
		
		final LearnerSimulator loader = new LearnerSimulator(new ByteArrayInputStream(output.toByteArray()),false);
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.name()),"AsomeData");
		}},IllegalArgumentException.class,"expecting to load a list with name ");
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
	public static String removeTagFromString(String str,ELEM_KINDS tag)
	{
		return removeTagFromString(str, tag.name());
	}
	
	/** Adds an extra attribute to the specified tag.
	 * 
	 * @param str XML to transform
	 * @param tag tag to modify
	 * @return modified XML
	 */
	public static String addExtraAttribute(String str,ELEM_KINDS tag)
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
	public static String breakNumericalValue(String str, ELEM_KINDS tag)
	{
		return str.replace(tag.name()+"=\"", tag.name()+"=\""+junkTag);
	}
	
	@Test
	public final void testCheckSingles1()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
		Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"A","B"}));
		ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
	}
	
	@Test
	public final void testCheckSingles2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
		ProgressDecorator.checkSingles(dumper.topElement, null);
	}
	
	@Test
	public final void testCheckSingles_fail1()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
		final Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"A","B"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail2()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		
		final Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"A","B"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail3()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		
		final Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"A","B"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail4()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
	
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, null);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail5()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
	
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, null);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail6()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
	
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, null);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail7()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		
		final Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"B"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
		}},IllegalArgumentException.class,"duplicate");
	}
	
	@Test
	public final void testCheckSingles_fail8()
	{
		ByteArrayOutputStream output = new ByteArrayOutputStream();
		final RecordProgressDecorator dumper = new RecordProgressDecorator(null,output,1,Configuration.getDefaultConfiguration(),false);
		Element A=null;
		A=dumper.doc.createElement("A");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("B");dumper.topElement.appendChild(A);
		A=dumper.doc.createElement("C");dumper.topElement.appendChild(A);
		
		final Set<String> singlesCollection = new TreeSet<String>();singlesCollection.addAll(Arrays.asList(new String[]{"A","B"}));
		checkForCorrectException(new whatToRun() { public void run() {
			ProgressDecorator.checkSingles(dumper.topElement, singlesCollection);
		}},IllegalArgumentException.class,"found unexpected elements");
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
