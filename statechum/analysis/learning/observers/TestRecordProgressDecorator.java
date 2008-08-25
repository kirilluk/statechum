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
	
	/** A modified version of a similar method from TestRpniLearner. This one
	 * checks that progress recording works correctly.
	 * 
	 * @param fsmString fsm to learn
	 * @param name its name
	 * @param plus positives
	 * @param minus negatives.
	 * @param useZip whether to use ZIP compression with the data stream. 
	 */
	protected void checkLearnerProgressRecording(String fsmString, String name,final String [][] plus, final String [][] minus)
	{
		checkLearnerProgressRecording(fsmString, name, plus, minus, false);
		checkLearnerProgressRecording(fsmString, name, plus, minus, true);
	}
	
	/** A modified version of a similar method from TestRpniLearner. This one
	 * checks that progress recording works correctly.
	 * 
	 * @param fsmString fsm to learn
	 * @param name its name
	 * @param plus positives
	 * @param minus negatives.
	 * @param useZip whether to use ZIP compression with the data stream. 
	 */
	protected void checkLearnerProgressRecording(String fsmString, String name,final String [][] plus, final String [][] minus, boolean useZip)
	{
		Configuration testConfig = Configuration.getDefaultConfiguration().copy();testConfig.setGdFailOnDuplicateNames(false);
		final DirectedSparseGraph g = TestFSMAlgo.buildGraph(fsmString, "sample FSM");
		final LearnerGraph expected = new LearnerGraph(g,testConfig);
		
		// now sanity checking on the plus and minus sets
		for(String [] path:plus)
			assert AbstractOracle.USER_ACCEPTED == expected.paths.tracePath(Arrays.asList(path));
		for(String [] path:minus)
			assert AbstractOracle.USER_ACCEPTED != expected.paths.tracePath(Arrays.asList(path));
		RPNIBlueFringeLearner l = new RPNIBlueFringeLearner(null,testConfig)
		{
			@Override
			public Pair<Integer,String> CheckWithEndUser(
					@SuppressWarnings("unused")	LearnerGraph model,
					List<String> question, 
					@SuppressWarnings("unused")	final Object [] moreOptions)
			{
				return new Pair<Integer,String>(expected.paths.tracePath(question),null);
			}
		};
		testConfig.setLearnerIdMode(IDMode.POSITIVE_NEGATIVE);
		ByteArrayOutputStream logStream = new ByteArrayOutputStream();

		RecordProgressDecorator recorder = new RecordProgressDecorator(l,logStream,1,Configuration.getDefaultConfiguration(),useZip);recorder.config=expected.config.copy();
		Collection<List<String>> testSet = new LinkedList<List<String>>();
		recorder.writeLearnerEvaluationData(new ProgressDecorator.LearnerEvaluationConfiguration(expected, testSet, testConfig, null));
		LearnerGraph learntStructureA = recorder.learnMachine(buildSet(plus), buildSet(minus));
		
		//System.out.println("compression rate: "+recorder.getCompressionRate());
		//System.out.println(logStream.toString()+"============");
		//System.out.println(logStream.toByteArray().length);
		LearnerGraph learntMachineNoRejects = Transform.removeRejectStates(learntStructureA,expected.config);
		WMethod.checkM(learntMachineNoRejects, expected);
		
		{// matching two simulators
			final LearnerSimulator 
				simulator = new LearnerSimulator(new ByteArrayInputStream(logStream.toByteArray()),useZip),
				simulator2 = new LearnerSimulator(new ByteArrayInputStream(logStream.toByteArray()),useZip);
			
			LearnerEvaluationConfiguration eval1 = simulator.readLearnerConstructionData();
			Assert.assertNull(WMethod.checkM(expected, eval1.graph));
			Assert.assertEquals(testSet, eval1.testSet);
			Assert.assertEquals(expected.config, simulator.config);
			LearnerEvaluationConfiguration eval2 = simulator2.readLearnerConstructionData();
			Assert.assertNull(WMethod.checkM(expected, eval2.graph));
			Assert.assertEquals(testSet, eval2.testSet);
			Assert.assertEquals(expected.config, simulator2.config);
			
			new Test_LearnerComparator(simulator,simulator2).learnMachine(buildSet(plus), buildSet(minus));
		}

		{// now a simulator to a learner
			final LearnerSimulator simulator = new LearnerSimulator(new ByteArrayInputStream(logStream.toByteArray()),useZip);
			LearnerEvaluationConfiguration eval1 = simulator.readLearnerConstructionData();
			Assert.assertNull(WMethod.checkM(expected, eval1.graph));
			Assert.assertEquals(testSet, eval1.testSet);
			Assert.assertEquals(expected.config, simulator.config);

			RPNIBlueFringeLearner learner2 = new RPNIBlueFringeLearner(null,expected.config)
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return new Pair<Integer,String>(expected.paths.tracePath(question),null);
				}
			};
			new Test_LearnerComparator(learner2,simulator).learnMachine(buildSet(plus), buildSet(minus));
		}


		{// now two learners
			RPNIBlueFringeLearner learnerA = new RPNIBlueFringeLearner(null,expected.config)
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return new Pair<Integer,String>(expected.paths.tracePath(question),null);
				}
			};
			RPNIBlueFringeLearner learnerB = new RPNIBlueFringeLearner(null,expected.config)
			{
				@Override
				public Pair<Integer,String> CheckWithEndUser(
						@SuppressWarnings("unused")	LearnerGraph model,
						List<String> question, 
						@SuppressWarnings("unused")	final Object [] moreOptions)
				{
					return new Pair<Integer,String>(expected.paths.tracePath(question),null);
				}
			};
			new Test_LearnerComparator(learnerA,learnerB).learnMachine(buildSet(plus), buildSet(minus));
		}

	}

	/** These tests check the framework to verify that 
	 * the outcome of learning is correct. The machines for the "testLearnerRec..." 
	 * tests are copied verbatim from TestRpniLearner
	 */
	@Test
	public void testLearnerRec1()
	{
		checkLearnerProgressRecording("A-a->B<-a-A\nA-b->A","testLearner1",
				new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b"}}, 
				new String[][]{new String[]{"a","b"},new String[]{"a","a"}});
	}

	@Test
	public void testLearnerRec2a()
	{
		checkLearnerProgressRecording("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n","testLearner2a",
				new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"}, new String[]{"b","c","c"}  }, 
				new String[][]{new String[]{"c"}});
	}

	@Test
	public void testLearnerRec2b()
	{
		checkLearnerProgressRecording("A-a->B<-a-C-b->A\nA-b->C\nC-c->C\n","testLearner2b",new String[][]{new String[]{"b","b","a"},new String[]{"b","a"},new String[]{"b","c"},new String[]{"b","c","c"}}, new String[][]{new String[]{"c"},new String[]{"b","b","c"}	});
	}

	@Test
	public void testLearnerRec3()
	{
		checkLearnerProgressRecording("A-text->B-text->B\nA-figure->C-figure->C\nB-figure->C\nC-text->B\nB-set_position->F\nF-edit->G\nG-finalize->A\nC-set_position->D\nD-set_dimensions->E-set_dimensions->E-figure->C\nE-text->B",
				"testLearner3",
				new String[][]{new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","set_dimensions", "figure", "set_position", "set_dimensions"}, 
				new String[]{"figure", "figure","set_position","set_dimensions","set_dimensions","set_dimensions","text", "set_position", "edit"}, 
				new String[]{"text","text","set_position","edit","finalize","text"}, 
				new String[]{"text","figure","figure"}, 
				new String[]{"text","text","set_position","edit","finalize","figure"}}, 
				
				new String[][]{
				new String[]{"text","text","set_position","edit","finalize","set_dimensions"},
				new String[]{"text","text","set_position","edit","finalize","set_position"}
		});
		
	}
}
