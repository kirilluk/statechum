package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.Helper.whatToRun;
import statechum.Label;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.AbstractPersistence;
import statechum.analysis.learning.rpnicore.LearnerGraph;
import statechum.analysis.learning.rpnicore.Transform;
import statechum.analysis.learning.rpnicore.WMethod;
import statechum.analysis.learning.rpnicore.WMethod.VERTEX_COMPARISON_KIND;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

public class TestErlangGraphs {
	
	protected Configuration config = Configuration.getDefaultConfiguration().copy();
	
	@Before
	public void beforeTest()
	{
		config.setLabelKind(LABELKIND.LABEL_ERLANG);
		ErlangModule.flushRegistry();
	}
	
	protected final String prefixFun = "{"+ErlangLabel.missingFunction+",";
	
	
	@Test
	public void testLabelBuilding1()
	{
		Assert.assertEquals(prefixFun+ "'a','b','c'}",((ErlangLabel)AbstractLearnerGraph.generateNewLabel("{a,b,c}", config)).toErlangTerm());
	}
	
	protected final String lblTextA = "{a,[b,g,56],\"c\"}", lblDumpA = prefixFun+ "'a',['b','g',56],\"c\"}";
	
	@Test
	public void testLabelBuilding2()
	{
		Assert.assertEquals(lblDumpA,((ErlangLabel)AbstractLearnerGraph.generateNewLabel(lblTextA, config)).toErlangTerm());
	}
	
	protected final String lblTextB = "{a,\"smth\",\"c\"}", lblDumpB = prefixFun+ "'a',\"smth\",\"c\"}";

	@Test
	public void testLabelBuilding3()
	{
		Assert.assertEquals(lblDumpB,((ErlangLabel)AbstractLearnerGraph.generateNewLabel(lblTextB, config)).toErlangTerm());
	}
	
	protected final String lblTextC = "{a,<< 156 >>}", lblDumpC = prefixFun+ "'a',<< 156>>}";

	@Test
	public void testLabelBuilding4()
	{
		Assert.assertEquals(lblDumpC,((ErlangLabel)AbstractLearnerGraph.generateNewLabel(lblTextC, config)).toErlangTerm());
	}
		
	@Test
	public void testLabelBuilding5()
	{
		Assert.assertEquals("{"+ErlangLabel.missingFunction+",'a','b','c'}",((ErlangLabel)AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'a','b','c'}", config)).toErlangTerm());
	}
	
	@Test
	public void testLabelBuilding6()
	{
		Assert.assertEquals("{"+ErlangLabel.missingFunction+",'a','b'}",((ErlangLabel)AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'a','b'}", config)).toErlangTerm());
	}
	
	@Test
	public void testLabelBuildingFail1()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{a,b,c", config).toErlangTerm();
		}},IllegalArgumentException.class,"unexpected end of tuple");
	}
	
	@Test
	public void testLabelBuildingFail2()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{a,b,c,d,e}", config).toErlangTerm();
		}},IllegalArgumentException.class,"arity 5");
	}
	
	@Test
	public void testLabelBuildingFail3()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{b}", config).toErlangTerm();
		}},IllegalArgumentException.class,"arity 1");
	}

	@Test
	public void testLabelBuildingFail4()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("b", config).toErlangTerm();
		}},IllegalArgumentException.class,"expected a tuple");
	}
	
	@Test
	public void testLabelBuildingFail5()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{\"a\",b,c}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should be an atom");
	}
	
	@Test
	public void testLabelBuildingFail6()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{98,c,d}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should be an atom");
	}

	@Test
	public void testLabelBuildingFail7()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{98,c}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should be an atom");
	}

	@Test
	public void testLabelBuildingFail8()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",56,c}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should be an atom");
	}

	@Test
	public void testLabelBuildingFail9()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",56,c,d}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should be an atom");
	}

	@Test
	public void testLabelBuildingFail10()
	{
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.generateNewLabel("{56,a,c,d}", config).toErlangTerm();
		}},IllegalArgumentException.class,"should start with "+ErlangLabel.missingFunction);
	}
	
	@Test
	public void testTraceParsing1()
	{
		List<Label> list = AbstractLearnerGraph.parseTrace("["+lblTextA+","+lblTextB+","+lblTextC+"]",config);
		Assert.assertEquals(lblDumpA,list.get(0).toErlangTerm());
		Assert.assertEquals(lblDumpB,list.get(1).toErlangTerm());
		Assert.assertEquals(lblDumpC,list.get(2).toErlangTerm());
		Assert.assertEquals(3,list.size());
	}

	@Test
	public void testTraceParsing2() throws IOException
	{
		String fnA = "{"+ErlangLabel.missingFunction+",'call','lock'}", fnB = "{"+ErlangLabel.missingFunction+",'call','unlock'}",comma=", ";
		String text = "["+fnA+comma+fnB+comma+fnB+"]";
		List<Label> list = AbstractLearnerGraph.parseTrace(text,config);
		ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(new File("ErlangExamples/locker/locker.erl")));
		List<String> newList = new LinkedList<String>();
		for(Label l:OTPBehaviour.convertTrace(OTPBehaviour.convertTrace(list, mod.behaviour.new ConverterErlToMod()), mod.behaviour.new ConverterModToErl()))
				newList.add(l.toErlangTerm());
		Assert.assertEquals(text,newList.toString());
	}
	
	@Test
	public void testTraceParsingFail1()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.parseTrace("b", config);
		}},IllegalArgumentException.class,"expected a sequence");		
	}
	
	@Test
	public void testTraceParsingFail2()
	{
		checkForCorrectException(new whatToRun() { public @Override void run() {
			AbstractLearnerGraph.parseTrace("["+lblTextA+","+lblTextB+",{},"+lblTextC+"]", config);
		}},IllegalArgumentException.class,"arity 0");
	}
	
	@Test
	public void testConstructErlGraph1()
	{
		LearnerGraph gr = buildLearnerGraph("A- "+lblTextA+" ->B<-"+lblTextA+"-C-"+lblTextB+"->A", "testConstructErlGraph1", config);
		Set<Label> alphabet = gr.pathroutines.computeAlphabet();
		Set<String> labels = new TreeSet<String>();
		Iterator<Label> iter = alphabet.iterator();
		labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
		Assert.assertFalse(iter.hasNext());
		Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'a',\"smth\",\"c\"}, {"+ErlangLabel.missingFunction+",'a',['b','g',56],\"c\"}]",labels.toString());
	}
	
	/** Tests that the same graph can be built by building string vertices and then converting them to Erlang. */
	@Test
	public void testConstructErlGraph2()
	{
		config.setLabelKind(LABELKIND.LABEL_STRING);
		LearnerGraph grStr = buildLearnerGraph("A- "+lblTextA+" ->B<-"+lblTextA+"-C-"+lblTextB+"->A", "testConstructErlGraph1", config);
		{
			Set<Label> alphabet = grStr.pathroutines.computeAlphabet();
			Set<String> labels = new TreeSet<String>();Iterator<Label> iter = alphabet.iterator();labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
			Assert.assertFalse(iter.hasNext());
			Assert.assertEquals("[{a,\"smth\",\"c\"}, {a,[b,g,56],\"c\"}]",labels.toString());
		}
		
		Configuration cnf = config.copy();cnf.setLabelKind(LABELKIND.LABEL_ERLANG);
		LearnerGraph grErl = grStr.transform.interpretLabelsOnGraph(new Transform.ConvertTypeOfLabels(cnf));
		{
			Set<Label> alphabet = grErl.pathroutines.computeAlphabet();
			Set<String> labels = new TreeSet<String>();Iterator<Label> iter = alphabet.iterator();labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
			Assert.assertFalse(iter.hasNext());
			Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'a',\"smth\",\"c\"}, {"+ErlangLabel.missingFunction+",'a',['b','g',56],\"c\"}]",labels.toString());
		}
	}
	
	@Test
	public void testConvertToModule1() throws IOException
	{
		LearnerGraph gr = buildLearnerGraph("A- {call, read} ->B<-{call, read}-C-{call, {write,aa}}->A", "testConstructErlGraph1", config);
		{
			Set<Label> alphabet = gr.pathroutines.computeAlphabet();
			Set<String> labels = new TreeSet<String>();Iterator<Label> iter = alphabet.iterator();labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
			Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'call','read'}, {"+ErlangLabel.missingFunction+",'call',{'write','aa'}}]",labels.toString());
		}
		File file = new File("ErlangExamples/locker/locker.erl");
		ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		final String expected = "[{{LOCKERPATH,25,handle_call,1,{'Func',[],[{'Alt',[],["+
			"{'Atom',[],['lock','read','unlock']},"+
			"{'Tuple',[],[{'Atom',[],['write']},{'Any',[]}]}]}],{'Any',[]}}},2,'call','read'}, "+
			"{{LOCKERPATH,25,handle_call,1,{'Func',[],[{'Alt',[],["+
			"{'Atom',[],['lock','read','unlock']},"+
			"{'Tuple',[],[{'Atom',[],['write']},{'Any',[]}]}]}],{'Any',[]}}},2,'call',{'write','aa'}}]";
		
		{
			LearnerGraph transformed = gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
			Set<Label> alphabet = transformed.pathroutines.computeAlphabet();
			StringBuffer quotedFileName = new StringBuffer();ErlangLabel.ErlangString.getSingleton().dump(file.getAbsolutePath(),quotedFileName);
			Set<String> labels = new TreeSet<String>();Iterator<Label> iter = alphabet.iterator();labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
			Assert.assertEquals(expected,
					labels.toString().replace(quotedFileName.toString(), "LOCKERPATH"));
		}
		{// try the same again
			LearnerGraph transformed = gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
			Set<Label> alphabet = transformed.pathroutines.computeAlphabet();
			StringBuffer quotedFileName = new StringBuffer();ErlangLabel.ErlangString.getSingleton().dump(file.getAbsolutePath(),quotedFileName);
			Set<String> labels = new TreeSet<String>();Iterator<Label> iter = alphabet.iterator();labels.add(iter.next().toErlangTerm());labels.add(iter.next().toErlangTerm());
			Assert.assertEquals(expected,
					labels.toString().replace(quotedFileName.toString(), "LOCKERPATH"));
		}
	}
	
	@Test
	public void testConvertToModuleFailure1() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph("A- {Acall, wibble} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		}},IllegalArgumentException.class,"unknown function");
	}
	
	/** Unknown function in module. */
	@Test
	public void testConvertToModuleFailure2() throws IOException
	{
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));config.setErlangModuleName(mod.getName());
		checkForCorrectException(new whatToRun() { public @Override void run() {
			buildLearnerGraph("A- {Acall, wibble} ->B", "testConvertToModuleFailure1", config);
		}},IllegalArgumentException.class,"unknown function \"Acall");
		
	}
	@Test
	public void testConvertToModuleForAnAlreadyAssignedLabel() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph("A- {call, read} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		final LearnerGraph converted = gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		LearnerGraph anotherGraph = converted.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		Assert.assertEquals(converted.pathroutines.computeAlphabet(),anotherGraph.pathroutines.computeAlphabet());
	}
	
	@Test
	public void testConvertToModuleForAnAlreadyAssignedToADifferentLabel() throws IOException
	{
		File file = new File("ErlangExamples/locker/locker.erl");
		Configuration moduleConfig = ErlangModule.setupErlangConfiguration(file);
		final ErlangModule mod = ErlangModule.loadModule(moduleConfig);
		final LearnerGraph gr = buildLearnerGraph("A- {call, read} ->B - {cast,yyy}->C", "testConvertToModuleFailure1", moduleConfig);
		for(Label lbl:gr.pathroutines.computeAlphabet())
			Assert.assertNotNull("undefined function for label "+lbl,((ErlangLabel)lbl).function);
		
		Iterator<Label> lblIter = gr.pathroutines.computeAlphabet().iterator();
		ErlangLabel lbl1 = (ErlangLabel)lblIter.next(),lbl2 = (ErlangLabel)lblIter.next();
		
		// now mess up the name of the function,
		FuncSignature fun2 = mod.sigs.get(lbl2.callName);
		mod.sigs.put(lbl1.callName,fun2);// mess up behaviour
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		}},IllegalArgumentException.class,"label already has a function assigned");
	}
	
	@Test
	public void testConvertToModuleFailure2b() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph("A- {call, wibble} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		
		Assert.assertEquals(gr.pathroutines.computeAlphabet(),
				gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl()).pathroutines.computeAlphabet());
	}
	
	@Test
	public void testConvertToModuleFailure2c() throws IOException
	{
		final LearnerGraph gr = buildLearnerGraph("A- {call, read} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		final LearnerGraph converted = gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		final LearnerGraph convertedBack = converted.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl());
		final LearnerGraph converted2 = gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		final LearnerGraph convertedBack2 = converted2.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl());
		Assert.assertNull(WMethod.checkM(converted,converted.getInit(),converted2,converted2.getInit(),VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNull(WMethod.checkM(convertedBack,convertedBack.getInit(),convertedBack2,convertedBack2.getInit(),VERTEX_COMPARISON_KIND.DEEP));
		Assert.assertNotNull(WMethod.checkM(converted,converted.getInit(),convertedBack,convertedBack.getInit(),VERTEX_COMPARISON_KIND.DEEP));
	}
	
	/** ErlToMod for text. */
	@Test
	public void testConvertToModuleFailure3() throws IOException
	{
		config.setLabelKind(LABELKIND.LABEL_STRING);
		final LearnerGraph gr = buildLearnerGraph("A- {call, wibble} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		}},IllegalArgumentException.class,"non-erlang labels");
	}

	/** ModToErl for text. */
	@Test
	public void testConvertToModuleFailure4() throws IOException
	{
		config.setLabelKind(LABELKIND.LABEL_STRING);
		final LearnerGraph gr = buildLearnerGraph("A- {call, wibble} ->B", "testConvertToModuleFailure1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		final ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		
		checkForCorrectException(new whatToRun() { public @Override void run() {
			gr.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl());
		}},IllegalArgumentException.class,"non-erlang labels");
	}

	
	@Test
	public void testSerialiseErlang() throws IOException
	{
		LearnerGraph grOrig = buildLearnerGraph("A- {call, read} ->B<-{call, read}-C-{call, {write,aa}}->A", "testConstructErlGraph1", config);
		File file = new File("ErlangExamples/locker/locker.erl");
		ErlangModule mod = ErlangModule.loadModule(ErlangModule.setupErlangConfiguration(file));
		LearnerGraph erlangGraph = grOrig.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		StringWriter writer = new StringWriter();
		// remove function delarations and serialise
		erlangGraph.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl()).storage.writeGraphML(writer);
		
		//for(Label l:erlangGraph.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterModToErl()).pathroutines.computeAlphabet()) System.out.println(l.toErlangTerm());
		LearnerGraph loaded = new LearnerGraph(config.copy());
		AbstractPersistence.loadGraph(new StringReader(writer.toString()), loaded);
		// add function declarations back
		LearnerGraph erlangLoadedGraph = loaded.transform.interpretLabelsOnGraph(mod.behaviour.new ConverterErlToMod());
		Assert.assertNull(WMethod.checkM(erlangGraph,erlangGraph.getInit(),erlangLoadedGraph,erlangLoadedGraph.getInit(),VERTEX_COMPARISON_KIND.DEEP));
	}	
}
