package statechum.analysis.learning;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.ProgressDecorator.ELEM_KINDS;

public class TestDumpProgressDecorator {

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
		StringWriter output = new StringWriter();
		ProgressDecorator dumper = new DumpProgressDecorator(null,output);
		org.w3c.dom.Element dataElem = dumper.addSequenceList("someData", data);
		
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = dumper.readSequenceList(dataElem,"someData");
		
		Assert.assertTrue(actual.equals(expected));
	}
	
	@Test
	public final void testWriteSequences2() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.close();
		
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString()));
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = loader.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.toString()),"someData");
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
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.topElement.appendChild(dumper.addSequenceList("moreData", data2));
		dumper.close();
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString()));
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
		}),actual = loader.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.toString()),"someData"),
		actual2 = loader.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.toString()),"moreData");
		Assert.assertTrue(actual.equals(expected));
		Assert.assertTrue(actual2.equals(expected2));
	}

	/** Invalid XML file. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteSequences_fail1() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.close();
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString().replaceFirst(ELEM_KINDS.ELEM_SEQ.toString(), "TT")));
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = loader.readSequenceList(loader.expectNextElement("whatever"),"someData");
		Assert.assertTrue(actual.equals(expected));
	}
	
	/** Expected tag not found */
	@Test
	public final void testWriteSequences_fail2() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.close();
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString().replaceAll(ELEM_KINDS.ELEM_SEQ.toString(), "TT")));
		try
		{
			loader.expectNextElement("U");
			Assert.fail("exception not thrown");
		}
		catch(org.junit.ComparisonFailure ex)
		{
			// ignore this - we expect expectNextElement to throw.
		}
	}
	/** Wrong tag when doing readSequenceList */
	@Test
	public final void testWriteSequences_fail3() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.close();
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString().replaceAll(ELEM_KINDS.ELEM_SEQ.toString(), "TT")));
		try
		{
			loader.readSequenceList(loader.expectNextElement("TT"),"someData");
			Assert.fail("exception not thrown");
		}
		catch(IllegalArgumentException ex)
		{
			Assert.assertTrue(ex.getMessage().contains("expecting to load a list of sequences"));
		}
	}

	/** The name given to the collection of sequences does not match. */
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteSequences4() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.topElement.appendChild(dumper.addSequenceList("someData", data));
		dumper.close();
		
		MatchProgressDecorator loader = new MatchProgressDecorator(null,new StringReader(output.toString()));
		loader.readSequenceList(loader.expectNextElement(ELEM_KINDS.ELEM_SEQ.toString()),"AsomeData");
	}
}
