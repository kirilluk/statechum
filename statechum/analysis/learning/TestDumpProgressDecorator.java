package statechum.analysis.learning;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import statechum.analysis.learning.DumpProgressDecorator.ELEM_KINDS;

public class TestDumpProgressDecorator {

	@Test
	public final void testWriteInputSequence1() {
		List<String> inputSequence = Arrays.asList(new String[] {});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+""+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence2() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test"});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+"this is a test"+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence3() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test","some more"});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+"this is a test"+DumpProgressDecorator.seqSep+"some more"+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test
	public final void testWriteInputSequence4() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test","some more","a"});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+
				"this is a test"+DumpProgressDecorator.seqSep+
				"some more"+DumpProgressDecorator.seqSep+
				"a"+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail1()  {
		List<String> inputSequence = Arrays.asList(new String[] {""+DumpProgressDecorator.seqStart});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+""+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail2() {
		List<String> inputSequence = Arrays.asList(new String[] {""+DumpProgressDecorator.seqNewLine});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+""+DumpProgressDecorator.seqEnd,wr.toString());
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail3() {
		List<String> inputSequence = Arrays.asList(new String[] {"this is a test",""});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+""+DumpProgressDecorator.seqEnd,wr.toString());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteInputSequence_fail4() {
		List<String> inputSequence = Arrays.asList(new String[] {""});
		Writer wr = new StringWriter();DumpProgressDecorator.writeInputSequence(wr, inputSequence);
		Assert.assertEquals(DumpProgressDecorator.seqStart+""+DumpProgressDecorator.seqEnd,wr.toString());
	}
	
	@Test
	public final void testReadInputSequence1() {
		List<String> expected = Arrays.asList(new String[] {});
		List<String> actual = DumpProgressDecorator.readInputSequence(new StringReader(
				""+DumpProgressDecorator.seqStart+DumpProgressDecorator.seqEnd));
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence2() {
		List<String> expected = Arrays.asList(new String[] {"some more"});
		List<String> actual = DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+"some more"+DumpProgressDecorator.seqEnd));
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence3() {
		List<String> expected = Arrays.asList(new String[] {"this is a test","some more"});
		List<String> actual = DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+"this is a test"+DumpProgressDecorator.seqSep+"some more"+DumpProgressDecorator.seqEnd));
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test
	public final void testReadInputSequence4() {
		List<String> expected = Arrays.asList(new String[] {"this is a test","some more","data"});
		List<String> actual = DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+"this is a test"+
				DumpProgressDecorator.seqSep+"some more"+
				DumpProgressDecorator.seqSep+"data"+
				DumpProgressDecorator.seqEnd));
		Assert.assertTrue("wrong result: "+actual,expected.equals(actual));		
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail2() {
		DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqEnd+"this is a test"+
				DumpProgressDecorator.seqSep+"some more"+
				DumpProgressDecorator.seqSep+"data"+
				DumpProgressDecorator.seqEnd));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail3() {
		DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+"this is a test"+
				DumpProgressDecorator.seqSep+"some more"+
				DumpProgressDecorator.seqSep+"data"));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail4() {
		DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+"this is a test"+
				DumpProgressDecorator.seqSep+
				DumpProgressDecorator.seqSep+"data"+
				DumpProgressDecorator.seqEnd));
	}

	@Test(expected=IllegalArgumentException.class)
	public final void testReadInputSequence_fail5() {
		DumpProgressDecorator.readInputSequence(new StringReader(
				DumpProgressDecorator.seqStart+
				DumpProgressDecorator.seqSep+"some more"+
				DumpProgressDecorator.seqSep+"data"+
				DumpProgressDecorator.seqEnd));
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
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		org.w3c.dom.Element dataElem = dumper.addSequenceList("someData", data);
		
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = dumper.readSequenceList(dataElem);
		
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
		dumper.addSequenceList("someData", data);
		dumper.close();
		DumpProgressDecorator loader = new DumpProgressDecorator(null,new StringReader(output.toString()));
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = loader.readSequenceList((org.w3c.dom.Element)loader.topElement.getFirstChild());
		Assert.assertTrue(actual.equals(expected));
	}

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
		dumper.addSequenceList("someData", data);
		dumper.close();
		DumpProgressDecorator loader = new DumpProgressDecorator(null,new StringReader(output.toString().replaceFirst(ELEM_KINDS.ELEM_SEQ.toString(), "TT")));
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = loader.readSequenceList((org.w3c.dom.Element)loader.topElement.getFirstChild());
		Assert.assertTrue(actual.equals(expected));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public final void testWriteSequences_fail2() {
		List<List<String>> data = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		});
		StringWriter output = new StringWriter();
		DumpProgressDecorator dumper = new DumpProgressDecorator(null,output);
		dumper.addSequenceList("someData", data);
		dumper.close();
		DumpProgressDecorator loader = new DumpProgressDecorator(null,new StringReader(output.toString().replaceAll(ELEM_KINDS.ELEM_SEQ.toString(), "TT")));
		List<List<String>> expected = TestFSMAlgo.buildList(new String[][]{
				new String[]{ "a","this is a test","3"},
				new String[]{},
				new String[]{},
				new String[]{"more data"}
		}), actual = loader.readSequenceList((org.w3c.dom.Element)loader.topElement.getFirstChild());
		Assert.assertTrue(actual.equals(expected));
	}
}
