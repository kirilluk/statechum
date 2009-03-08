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

package statechum.apps;

import java.io.StringReader;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Test;

import statechum.Configuration;
import statechum.Pair;
import statechum.Configuration.IDMode;
import statechum.analysis.learning.rpnicore.FsmParser;
import statechum.analysis.learning.rpnicore.LabelRepresentation;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.TestFSMAlgo;

import static statechum.Helper.checkForCorrectException;
import static statechum.Helper.whatToRun;
import static statechum.analysis.learning.rpnicore.LabelRepresentation.ENDL;

/**
 * This one tests the ability of QSMTool to load a configuration.
 * 
 * @author kirr
 *
 */
public class TestQSMTool {

	@Test
	public final void testLoad1()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(""));
		Assert.assertEquals(-1,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad2()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a"));
		Assert.assertEquals(-1,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad3()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n+ a b\n- q er t y"));
		Assert.assertEquals(-1,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}, new String[]{"a","b"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}, new String[]{"q","er","t","y"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	/** Repeated sequence. */
	@Test
	public final void testLoad4()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n+ a b\n- q er t y\n- q er t y\n- q er t y"));
		Assert.assertEquals(-1,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}, new String[]{"a","b"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}, new String[]{"q","er","t","y"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad5()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n# another comment\nk 8\n\n"));
		Assert.assertEquals(8,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad6()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n# another comment\n"+
				"k 8\n\npassive"));
		Assert.assertEquals(8,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(false,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad7()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n"+
				QSMTool.cmdConfig+" attenuationK 0.34\n"+
				QSMTool.cmdConfig+" defaultInitialPTAName test\n"+
				QSMTool.cmdConfig+" compressLogs true\n"+
				QSMTool.cmdConfig+" learnerIdMode NONE"));
		Assert.assertEquals(-1,tool.k);
		Configuration expectedConfig = Configuration.getDefaultConfiguration().copy();
		expectedConfig.setAttenuationK(0.34);
		expectedConfig.setDefaultInitialPTAName("test");
		expectedConfig.setCompressLogs(true);
		expectedConfig.setLearnerIdMode(IDMode.NONE);
		Assert.assertEquals(expectedConfig,tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad8()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n"+
				QSMTool.cmdConfig+" attenuationK 0.34\n"+
				QSMTool.cmdConfig+" defaultInitialPTAName test\n"+
				QSMTool.cmdConfig+" compressLogs false\n"+
				QSMTool.cmdConfig+" learnerIdMode NONE\n"+
				QSMTool.cmdLTL+" this is a test\n"+
				QSMTool.cmdIFTHENAUTOMATON+" A-b->B == THEN = C-a->C\n"+
				QSMTool.cmdIFTHENAUTOMATON+" A-a->B == THEN = C-b->C\n"+
				QSMTool.cmdLTL+" more test"
		));
		Assert.assertEquals(-1,tool.k);
		Configuration expectedConfig = Configuration.getDefaultConfiguration().copy();
		expectedConfig.setAttenuationK(0.34);
		expectedConfig.setDefaultInitialPTAName("test");
		expectedConfig.setCompressLogs(false);
		expectedConfig.setLearnerIdMode(IDMode.NONE);
		Assert.assertEquals(expectedConfig,tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Set<String> expectedltl = new TreeSet<String>();expectedltl.addAll(Arrays.asList(new String[]{
				QSMTool.cmdLTL+" this is a test",QSMTool.cmdIFTHENAUTOMATON+" A-b->B == THEN = C-a->C",
				QSMTool.cmdIFTHENAUTOMATON+" A-a->B == THEN = C-b->C",QSMTool.cmdLTL+" more test"}));
		Assert.assertEquals(expectedltl,tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testLoad9()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n"+
				QSMTool.cmdConfig+" attenuationK 0.34\n"+
				QSMTool.cmdConfig+" defaultInitialPTAName test\n"+
				QSMTool.cmdConfig+" compressLogs true\n"+
				QSMTool.cmdConfig+" learnerIdMode NONE\n"+
				QSMTool.cmdDotOutput+"\n"+QSMTool.cmdTextOutput
		));
		Assert.assertEquals(-1,tool.k);
		Configuration expectedConfig = Configuration.getDefaultConfiguration().copy();
		expectedConfig.setAttenuationK(0.34);
		expectedConfig.setDefaultInitialPTAName("test");
		expectedConfig.setCompressLogs(true);
		expectedConfig.setLearnerIdMode(IDMode.NONE);
		expectedConfig.setGenerateDotOutput(true);expectedConfig.setGenerateTextOutput(true);
		Assert.assertEquals(expectedConfig,tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(true,tool.active);
		Assert.assertNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	@Test
	public final void testWrongCommand()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("A# sample file"));
		}},IllegalArgumentException.class,"invalid command");
	}
	
	@Test
	public final void testEmpty1()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("+"));
		}},IllegalArgumentException.class,"Argument required");
	}
	
	@Test
	public final void testEmpty2()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("-"));
		}},IllegalArgumentException.class,"Argument required");
	}
	
	@Test
	public final void testEmpty3()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdLTL));
		}},IllegalArgumentException.class,"Argument required");
	}
	
	@Test
	public final void testInsufficientArgsForProperty1()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig));
		}},IllegalArgumentException.class,"Argument required");
	}
	
	@Test
	public final void testInsufficientArgsForProperty2()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" a"));
		}},IllegalArgumentException.class,"invalid configuration");
	}
	
	@Test
	public final void testWrongProperty()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" junk test"));
		}},IllegalArgumentException.class,"cannot deserialise");
	}
	
	/** Wrong enum value. */
	@Test
	public final void testWrongValueForProperty1()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" learnerIdMode JUNK"));
		}},IllegalArgumentException.class,"failed to load");
	}
	
	/** Cannot parse text as an integer. */
	@Test
	public final void testWrongValueForProperty2()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" klimit 2.0"));
		}},IllegalArgumentException.class,"failed to load");
	}
	
	/** Cannot parse text as a double. */
	@Test
	public final void testWrongValueForProperty3()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" gdLowToHighRatio AAA34"));
		}},IllegalArgumentException.class,"failed to load");
	}
	
	/** Invalid value for a number. */
	@Test
	public final void testWrongValueForProperty4()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(QSMTool.cmdConfig+" gdLowToHighRatio 34"));
		}},IllegalArgumentException.class,"failed to load");
	}
	
	/** Invalid value for a number. */
	@Test
	public final void testWrongValueForK()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("k 2.0"));
		}},NumberFormatException.class,"");
	}
	
	/** Loading of labels. No data provided in label descriptions*/
	@Test
	public final void testLoadXMLabels1()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader("# sample file\n+ part_a part_b part_c\n- smth_a\n# another comment\n"+
		"k 8\n\npassive\n"+QSMTool.cmdOperation+"\n"+QSMTool.cmdOperation+" "+LabelRepresentation.INITMEM+" "+LabelRepresentation.OP_DATA.PRE+" decl_N"+"\n"+QSMTool.cmdOperation));
		Assert.assertEquals(8,tool.k);
		Assert.assertEquals(Configuration.getDefaultConfiguration(),tool.learnerInitConfiguration.config);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"part_a","part_b","part_c"}}),tool.sPlus);
		Assert.assertEquals(TestFSMAlgo.buildSet(new String[][]{new String[]{"smth_a"}}),tool.sMinus);
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertEquals(false,tool.active);
		Assert.assertNotNull(tool.learnerInitConfiguration.labelDetails);
	}
	
	/** Loading of labels. Valid data is provided in label descriptions. */
	@Test
	public final void testLoadXMLabels2()
	{
		QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(
			QSMTool.cmdOperation+" "+LabelRepresentation.INITMEM+" "+LabelRepresentation.OP_DATA.PRE+" decl"+LabelRepresentation.delimiterString+"N"+"\n"+
			QSMTool.cmdOperation+" "+LabelRepresentation.INITMEM+" "+LabelRepresentation.OP_DATA.POST+" constraint"+LabelRepresentation.delimiterString+"N"+"\n"+
			QSMTool.cmdOperation+" a "+LabelRepresentation.OP_DATA.POST+" constraint_N"+"\n"+
			QSMTool.cmdOperation));
		Assert.assertNull(tool.learnerInitConfiguration.ifthenSequences);
		Assert.assertNotNull(tool.learnerInitConfiguration.labelDetails);
		tool.learnerInitConfiguration.labelDetails.buildVertexToAbstractStateMap(new LearnerGraph(FsmParser.buildGraph("A-a->B", "testLoadXMLabels2"),Configuration.getDefaultConfiguration()), null);
		Pair<String,String> state = tool.learnerInitConfiguration.labelDetails.getConjunctionForPath(Arrays.asList(new LabelRepresentation.Label[]{}),null);
		Assert.assertEquals("decl"+LabelRepresentation.delimiterString+"2"+ENDL,state.firstElem);
		Assert.assertEquals(LabelRepresentation.commentForNewSeq+"[]"+ENDL+
				"(and"+ENDL+
				LabelRepresentation.commentForInit+ENDL+
				"constraint"+LabelRepresentation.delimiterString+"2"+ENDL+
				')'+ENDL,
				state.secondElem);
	}
	
	/** Loading of labels. Invalid data provided in label descriptions*/
	@Test
	public final void testLoadXMLabels_Error()
	{
		checkForCorrectException(new whatToRun() { public void run() {
			QSMTool tool = new QSMTool();tool.loadConfig(new StringReader(
				QSMTool.cmdOperation+" "+LabelRepresentation.INITMEM+" "+LabelRepresentation.OP_DATA.PRE+" decl_N"+"\n"+
				QSMTool.cmdOperation+" "+LabelRepresentation.INITMEM+" JUNK constraint_N"+"\n"+
				QSMTool.cmdOperation));
		}},IllegalArgumentException.class,"expected [PRE");
	}
	
	
}
