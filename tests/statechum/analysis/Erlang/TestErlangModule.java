/* Copyright (c) 2011 Ramsay Taylor and Kirill Bogdanov
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

package statechum.analysis.Erlang;

import static statechum.Helper.checkForCorrectException;
import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.Configuration;
import statechum.GlobalConfiguration;
import statechum.Configuration.LABELKIND;
import statechum.GlobalConfiguration.G_PROPERTIES;
import statechum.Helper;
import statechum.Helper.whatToRun;
import statechum.Label;
import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.Erlang.ErlangRunner.ErlangThrownException;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.Erlang.Signatures.TestTypes;
import statechum.analysis.learning.ErlangOracleLearner;
import statechum.analysis.learning.ErlangOracleLearner.TraceOutcome;
import statechum.analysis.learning.ErlangOracleLearner.TraceOutcome.TRACEOUTCOME;
import statechum.analysis.learning.RPNILearner;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.observers.ProgressDecorator.LearnerEvaluationConfiguration;
import statechum.analysis.learning.rpnicore.AbstractLearnerGraph;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.analysis.learning.rpnicore.LearnerGraph;

public class TestErlangModule 
{
	/** This one is used to number work directories so that different tests do not affect each other. Unfortunately, the numbering is sequential hence it is not known which test corresponds to which number. */
	protected static int number = 0;

	/** URL of the writable directory to be used for tests. */
	public File testDir = null;

	public final String ErlangExamples = GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.PATH_ERLANGEXAMPLES);
	protected Configuration config = Configuration.getDefaultConfiguration().copy();
	protected ErlangRunner runner = null;
	protected static ErlangRuntime erlRuntime;
	
	public TestErlangModule()
	{
	}

	@BeforeClass
	public static void beforeClass()
	{
		erlRuntime = new ErlangRuntime();erlRuntime.setTimeout(500);erlRuntime.startRunner();		
	}
	
	@AfterClass
	public static void afterClass()
	{
		erlRuntime.killErlang();
	}
	
	@Before
	public void beforeTest()
	{
		testDir = new File(GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP),"__TestErlangRunner__"+(number++));
		runner = erlRuntime.createNewRunner();config.setErlangMboxName(runner.getRunnerName()); 
		erlangFile=testDir.getAbsolutePath()+File.separator+"testFile.erl";
		erlangFileOther = testDir.getAbsolutePath()+File.separator+"testFileOther.erl";
		ErlangModule.flushRegistry();
		if (!testDir.isDirectory())
		{
			Assert.assertTrue("could not create "+testDir.getAbsolutePath(),testDir.mkdir());
		}
	}
	
	@After
	public void afterTest()
	{
		runner.close();
		ExperimentRunner.zapDir(testDir);
	}

	@Test
	public void testRunParserFailure1()
	{
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
		public @Override void run() throws IOException {
			ErlangModule.setupErlangConfiguration(config,new File(ErlangExamples,"WibbleMonster/wibble.erla"));
		}},IllegalArgumentException.class,"Invalid module");
	}

	/** This is one of those odd tests which behaves differently on different operating systems,
	 * hence there are two different cases.
	 * @throws IOException 
	 */
	@Test
	public void testRunParserFailure2() throws IOException
	{
		final String wibbleDir = ErlangExamples+File.separator+"WibbleMonster";
		final File fileWibble = new File(wibbleDir,"Wibble.erl"),
			filewibble = new File(wibbleDir,"wibble.erl");
		if (!fileWibble.canRead()) // running on a case-sensitive OS
			statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { 
				public @Override void run() throws IOException {
					Configuration configWibble = config.copy();ErlangModule.setupErlangConfiguration(configWibble,fileWibble);configWibble.setErlangCompileIntoBeamDirectory(true);
					ErlangModule.loadModule(configWibble);
				}},IOException.class,"does not exist");
		else
		{
			File beam = new File(ErlangRunner.getName(fileWibble, ERL.BEAM, true));
			if (beam.canRead()) Assert.assertTrue(beam.delete());
			final Configuration configwibble = config.copy();ErlangModule.setupErlangConfiguration(configwibble,filewibble);configwibble.setErlangCompileIntoBeamDirectory(true);
			ErlangModule.loadModule(configwibble,true);
			
			// At this point, I have wibble compiled using a correct file name, then I delete 
			// a plt if it exists and try to generate a new one by forcefully reloading a module.
			// This process checks the name of the file and throws an exception if it is not consistent
			// with the file name.
			
			final Configuration configWibble = config.copy();ErlangModule.setupErlangConfiguration(configWibble,fileWibble);configWibble.setErlangCompileIntoBeamDirectory(true);
			File plt = new File(ErlangRunner.getName(fileWibble, ERL.PLT, configWibble.getErlangCompileIntoBeamDirectory()));
			if (plt.canRead()) Assert.assertTrue(plt.delete());
			statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { 
				public @Override void run() throws IOException {
					ErlangModule.loadModule(configWibble,true);
			}},ErlangThrownException.class,"Invalid file name");
		}
	}

    /** Only used for testing against runTyperAsAProcessInsideErlang. 
     * @throws IOException */
    protected static String runTyperAsAProcess(File f, ErlangRunner runner) throws IOException
    {
    	ErlangRunner.compileErl(f, runner, true);
    	
        {// Now build environment variables to ensure that dialyzer will find a directory to put its plt file in.
	        Map<String,String> environment = System.getenv();
	        String [] envp = new String[environment.size()+1];int i=0;
	        for(Entry<String,String> entry:System.getenv().entrySet())
	        	envp[i++]=entry.getKey()+"="+entry.getValue();envp[i++]="HOME="+f.getParentFile().getAbsolutePath();
	
	        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"dialyzer","--build_plt","--output_plt",ErlangRunner.getName(f, ERL.PLT,true),ErlangRunner.getName(f, ERL.BEAM,true)}, envp);//, ErlangRunner.getErlangBeamDirectory());
	        ErlangRuntime.dumpProcessOutputOnFailure("dialyzer",p);
        }
        // Receive the type info....
        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"typer","--plt",ErlangRunner.getName(f, ERL.PLT, true),ErlangRunner.getName(f, ERL.ERL,false)}, null);//, ErlangRunner.getErlangBeamDirectory());
    	final StringBuffer err=new StringBuffer(),out=new StringBuffer(); 
        ExperimentRunner.dumpStreams(p, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(StringBuffer b) {
                err.append(b);
            }

            @Override
            public void StdOut(StringBuffer b) {
            	out.append(b);
            }
        });
        try {
            p.waitFor();
        } catch (InterruptedException e) {
        }
        if (p.exitValue() != 0)
        	throw new IllegalArgumentException("Failure running "+f.getName()+"\n"+err+(err.length()>0?"\n":"")+out);

       return out.toString();
    }
    
    /** Only used for testing against runTyperAsAProcess. 
     * @throws IOException */
    protected String runDialyzerAndTyperAsAProcessInsideErlang(File f) throws IOException
    {
    	ErlangRunner.compileErl(f, runner, true);
    	OtpErlangObject otpArgs[] = new OtpErlangObject[]{
        		null, 
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.BEAM, true))}),
				new OtpErlangString(ErlangRunner.getName(f, ERL.PLT, true)),
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.ERL, false))}),
				new OtpErlangAtom("text")
			};
    	otpArgs[0]=new OtpErlangAtom("dialyzer");
    	runner.call(otpArgs,"Could not run dialyzer");
    	otpArgs[0]=new OtpErlangAtom("typer");
    	OtpErlangTuple response = runner.call(otpArgs,"Could not run typer");
    	return ((OtpErlangString)response.elementAt(1)).stringValue();
    }
    
    /** Only used for testing against runTyperAsAProcess. 
     * @throws IOException */
    protected String runOnlyTyperAsAProcessInsideErlang(File f) throws IOException
    {
    	ErlangRunner.compileErl(f, runner, true);
    	OtpErlangObject otpArgs[] = new OtpErlangObject[]{
        		null, 
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.BEAM, true))}),
				new OtpErlangString(ErlangRunner.getName(f, ERL.PLT, true)),
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.ERL, false))}),
				new OtpErlangAtom("text")
			};
    	otpArgs[0]=new OtpErlangAtom("typer");
    	OtpErlangTuple response = runner.call(otpArgs,"Could not run typer");
    	return ((OtpErlangString)response.elementAt(1)).stringValue();
    }

    /** Loads the contents of a file into a string. 
     * @param file file to load
     * 
     * @throws IOException 
     */
    private static String loadFile(File file) throws IOException
    {
    	BufferedReader input = new BufferedReader(new FileReader(file));
    	StringBuffer result = new StringBuffer();
		String line;
		while ((line = input.readLine()) != null) {
			result.append(line);result.append('\n');
		}
		input.close();return result.toString();
    }
    
    public void testConsistencyBetweenOriginalAndOurTyperHelper(File origFile) throws IOException
    {
    	Assert.assertFalse(erlangFile.equals(erlangFileOther));
    	String moduleName = ErlangRunner.getName(origFile, ERL.MOD, true);
    	final String someErlang = loadFile(origFile);
  		String typerInRunner = null, typerAsProcess = null;
  		{// we have to create new files because of file sync problem between multiple instances of Erlang - where multiple tests are run, we sometime get module_info, sometimes not.
  			Assert.assertTrue(new File(testDir.getAbsolutePath()+File.separator+"A").mkdir());
  			String fileA = testDir.getAbsolutePath()+File.separator+"A"+File.separator+moduleName+".erl";
  			Writer wr = new FileWriter(fileA);wr.write(someErlang);wr.close();
			typerInRunner = runDialyzerAndTyperAsAProcessInsideErlang(new File(fileA)).replace("\\\\","\\").replace(fileA,"FileName");
  		}
  		{
  			Assert.assertTrue(new File(testDir.getAbsolutePath()+File.separator+"B").mkdir());
  			String fileB = testDir.getAbsolutePath()+File.separator+"B"+File.separator+moduleName+".erl";
  			Writer wr = new FileWriter(fileB);wr.write(someErlang);wr.close();
  			typerAsProcess = runTyperAsAProcess(new File(fileB),runner).replace("\\\\","\\").replace(fileB, "FileName");
  		}
	  		typerInRunner = typerInRunner.replaceAll("-spec module_info.*\n", "");
	  		typerInRunner = typerInRunner.replaceAll(".*Unknown functions: \\[\\{erlang,get_module_info,1\\},\\{erlang,get_module_info,2\\}\\].*\n", "");
	  		typerAsProcess = typerAsProcess.replaceAll("-spec module_info.*\n", "");
	  		typerAsProcess = typerAsProcess.replaceAll(".*Unknown functions: \\[\\{erlang,get_module_info,1\\},\\{erlang,get_module_info,2\\}\\].*\n", "");//[{erlang,get_module_info,1},{erlang,get_module_info,2}]
		Assert.assertEquals(typerAsProcess,typerInRunner);
    }
    
    @Test
    public void testConsistencyBetweenOriginalAndOurTyper1() throws IOException
    {
    	testConsistencyBetweenOriginalAndOurTyperHelper(new File(ErlangExamples,"WibbleMonster/wibble.erl"));
    }
    
    @Test
    public void testConsistencyBetweenOriginalAndOurTyper2() throws IOException
    {
    	// for some reason, Erlang runner does not add module_info information
    	testConsistencyBetweenOriginalAndOurTyperHelper(new File(ErlangExamples,"locker/locker.erl"));
    }
    
    @Test
    public void testTyperUnreadableBeamFile()
    {
    	final File f = new File(ErlangExamples,"locker/locker.erl");
    	new File(ErlangRunner.getName(f, ERL.PLT,true)).delete();new File(ErlangRunner.getName(f, ERL.BEAM,true)).delete();
       	final OtpErlangObject otpArgs[] = new OtpErlangObject[]{
        		null, 
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.BEAM, true))}),
				new OtpErlangString(ErlangRunner.getName(f, ERL.PLT, true)),
				new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.ERL, false))}),
				new OtpErlangAtom("text")
			};
    	otpArgs[0]=new OtpErlangAtom("dialyzer");
		Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
			public @Override void run() throws IOException {
				runner.call(otpArgs,"Could not run dialyzer");
			}},ErlangThrownException.class,"Failed to obtain file info");
    }
    
    @Test
    public void testTyperWithInvalidPLTerror() throws IOException
    {
    	final File file = new File(ErlangExamples,"locker/locker.erl");
    	new File(ErlangRunner.getName(file, ERL.PLT,true)).delete();
    	Writer wr = new FileWriter(ErlangRunner.getName(file, ERL.PLT,true));wr.write("junk");wr.close();
		Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
			public @Override void run() throws IOException {
				runOnlyTyperAsAProcessInsideErlang(file);
			}},ErlangThrownException.class,"has thrown");
    	new File(ErlangRunner.getName(file, ERL.BEAM, true)).delete();
    	new File(ErlangRunner.getName(file, ERL.PLT, true)).delete();
    }
    
    @Test
    public void testTyperWithInvalidPLT() throws IOException
    {
    	File file = new File(ErlangExamples,"locker/locker.erl");
    	ErlangModule.setupErlangConfiguration(config,file);
    	config.setErlangCompileIntoBeamDirectory(true);
    	new File(ErlangRunner.getName(file, ERL.PLT,config.getErlangCompileIntoBeamDirectory())).delete();
    	Writer wr = new FileWriter(ErlangRunner.getName(file, ERL.PLT,config.getErlangCompileIntoBeamDirectory()));wr.write("junk");wr.close();
    	ErlangModule.loadModule(config);// this should work by rebuilding plt ...
    	runOnlyTyperAsAProcessInsideErlang(file);// ... check that it did this.
    }
    
    protected final Configuration defaultConfig = Configuration.getDefaultConfiguration().copy();
    
    @Test
    public void testExtractFunctionTypes1() throws IOException
    {
    	GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP);
    	File file = new File(ErlangExamples,"WibbleMonster/wibble.erl");
    	ErlangModule.setupErlangConfiguration(config,file);
    	ErlangModule mod = ErlangModule.loadModule(config);
    	Assert.assertTrue(mod.behaviour instanceof OTPGenServerBehaviour);
    	Assert.assertTrue(mod.behaviour.dependencies.isEmpty());
    	for(FuncSignature s:mod.sigs.values())
    	{
       		FuncSignature newSig = new FuncSignature(defaultConfig, ErlangLabel.parseText(s.toErlangTerm()),null);
       		Assert.assertEquals(s, newSig);
       		Assert.assertEquals(s, new FuncSignature(defaultConfig, ErlangLabel.parseText(newSig.toErlangTerm()),null));
    	}
    }
    
    @Test
    public void testExtractFunctionTypes2() throws IOException
    {
    	GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP);
    	File file = new File(ErlangExamples,"locker/locker.erl");
    	ErlangModule.setupErlangConfiguration(config,file);
    	ErlangModule mod = ErlangModule.loadModule(config);
    	Assert.assertTrue(mod.behaviour instanceof OTPGenServerBehaviour);
    	Assert.assertTrue(mod.behaviour.dependencies.isEmpty());
    	for(FuncSignature s:mod.sigs.values())
    	{
       		FuncSignature newSig = new FuncSignature(defaultConfig, ErlangLabel.parseText(s.toErlangTerm()),null);
       		Assert.assertEquals(s, newSig);
       		Assert.assertEquals(s, new FuncSignature(defaultConfig, ErlangLabel.parseText(newSig.toErlangTerm()),null));
    	}
    }
    
   	@Test
   	public void testWibbleAlphabet() throws IOException
    {
    	ErlangModule.setupErlangConfiguration(config,new File(ErlangExamples,"WibbleMonster/wibble.erl"));
   		ErlangModule mod = ErlangModule.loadModule(config);
   		Assert.assertEquals("[" +
   				"{"+ErlangLabel.missingFunction+",'call','xyz','here_kirill'}," +
   				"{"+ErlangLabel.missingFunction+",'call','xyz','listing'}," +
   				"{"+ErlangLabel.missingFunction+",'call','xyz','wibbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call','xyz','wobbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble'],'here_kirill'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble'],'listing'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble'],'wibbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble'],'wobbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble' | 'AnyListElemWibble'],'here_kirill'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble' | 'AnyListElemWibble'],'listing'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble' | 'AnyListElemWibble'],'wibbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble' | 'AnyListElemWibble'],'wobbling'}," +
    			"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble'],'here_kirill'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble'],'listing'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble'],'wibbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble'],'wobbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble' | 'AnyListElemWibble'],'here_kirill'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble' | 'AnyListElemWibble'],'listing'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble' | 'AnyListElemWibble'],'wibbling'}," +
   				"{"+ErlangLabel.missingFunction+",'call',['AnyListElemWibble','AnyListElemWibble' | 'AnyListElemWibble'],'wobbling'}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast','JustAnythingA',[]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast','JustAnythingA',[128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast','JustAnythingA',[128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast','JustAnythingA',[128,128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',[],[]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',[],[128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',[],[128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',[],[128,128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA'],[]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA'],[128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA'],[128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA'],[128,128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA','WobbleA'],[]},"+
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA','WobbleA'],[128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA','WobbleA'],[128,128]},"+
   		   		"{"+ErlangLabel.missingFunction+",'cast',['WibbleA','WobbleA'],[128,128,128]}," +
   		   		"{"+ErlangLabel.missingFunction+",'info','JustAnythingA',{'noreply','JustAnythingA'}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info','JustAnythingA',{'noreply',[]}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info','JustAnythingA',{'noreply',['WibbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info','JustAnythingA',{'noreply',['WibbleA','WobbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',[],{'noreply','JustAnythingA'}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',[],{'noreply',[]}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',[],{'noreply',['WibbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',[],{'noreply',['WibbleA','WobbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA'],{'noreply','JustAnythingA'}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA'],{'noreply',[]}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA'],{'noreply',['WibbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA'],{'noreply',['WibbleA','WobbleA']}}," + 
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA','WobbleA'],{'noreply','JustAnythingA'}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA','WobbleA'],{'noreply',[]}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA','WobbleA'],{'noreply',['WibbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'info',['WibbleA','WobbleA'],{'noreply',['WibbleA','WobbleA']}}," +
   		   		"{"+ErlangLabel.missingFunction+",'init','JustAnythingA','ok'}," + 
   		   		"{"+ErlangLabel.missingFunction+",'init',[],'ok'}," +
   		   		"{"+ErlangLabel.missingFunction+",'init',['WibbleA'],'ok'}," + 
   		   		"{"+ErlangLabel.missingFunction+",'init',['WibbleA','WobbleA'],'ok'}" +
   		   		"]",
   				TestTypes.getAlphabetAsString(mod));
    }
    
    @Test
    public void testAttemptTracesNotInAlphabet()
    {
    	GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP);
       	LearnerEvaluationConfiguration evalConf = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
    	evalConf.config = config.copy();
    	final String moduleName = "locker";
    	evalConf.config.setErlangModuleName(moduleName);
    	evalConf.config.setErlangSourceFile(new File(ErlangExamples+File.separator+"locker",moduleName + ".erl"));
    	evalConf.config.setLabelKind(LABELKIND.LABEL_ERLANG);
    	final ErlangOracleLearner learner = new ErlangOracleLearner(null, evalConf);
    	
    	// The above loads a module, this one gets that module and subsequently updates its alphabet.
    	ErlangModule mod = ErlangModule.findModule(evalConf.config.getErlangModuleName());
    	
    	final ErlangLabel initLabel = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'}", evalConf.config,evalConf.getLabelConverter())), 
			labelLock = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}}", evalConf.config,evalConf.getLabelConverter()));
		final ErlangLabel labelInvalidRead = new ErlangLabel(labelLock.function,labelLock.callName,labelLock.input,new OtpErlangInt(88));
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
			public @Override void run() {
				learner.askErlang(Arrays.asList(new Label[]{initLabel,labelLock,labelInvalidRead}));
			}},IllegalArgumentException.class,"does not belong");
    }
    
    @Test
    public void testAttemptTracesWrongModule() throws IOException
    {
    	GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP);
       	LearnerEvaluationConfiguration evalConf = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
       	evalConf.config = config.copy();ErlangModule.setupErlangConfiguration(evalConf.config,new File(ErlangExamples,"locker/locker"+ErlangRunner.ERL.ERL.toString()));
    	final ErlangOracleLearner learner = new ErlangOracleLearner(null, evalConf);
    	
    	// The above loads a module, this one gets that module and subsequently updates its alphabet.
    	Configuration exporterConfiguration = config.copy();ErlangModule.setupErlangConfiguration(exporterConfiguration,new File(ErlangExamples,"exporter/exporter"+ErlangRunner.ERL.ERL.toString()));
     	
    	ErlangModule.findModule(evalConf.config.getErlangModuleName());
    	ErlangModule modExporter = ErlangModule.loadModule(exporterConfiguration);
    	
    	final ErlangLabel pushLabel = modExporter.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel(
    			"{"+ErlangLabel.missingFunction+",'exporter:push/1',['JustAnythingA'],'ok'}", exporterConfiguration,evalConf.getLabelConverter()));
		//final ErlangLabel labelInvalidArity = new ErlangLabel(initLabel.function,initLabel.callName,initLabel.input,initLabel.expectedOutput);
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
			public @Override void run() {
				learner.askErlang(Arrays.asList(new Label[]{pushLabel}));
			}},IllegalArgumentException.class,"but attempting to call");
    }
    
    @Test
    public void testAttemptTraces()
    {
    	GlobalConfiguration.getConfiguration().getProperty(G_PROPERTIES.TEMP);
    	LearnerEvaluationConfiguration evalConf = new LearnerEvaluationConfiguration(Configuration.getDefaultConfiguration().copy());
    	final String moduleName = "locker";
    	evalConf.config = config.copy();ErlangModule.setupErlangConfiguration(evalConf.config,new File(ErlangExamples+File.separator+"locker",moduleName + ErlangRunner.ERL.ERL.toString()));
    	ErlangOracleLearner learner = new ErlangOracleLearner(null, evalConf);
    	
    	// The above loads a module, this one gets that module and subsequently updates its alphabet.
    	ErlangModule mod = ErlangModule.findModule(moduleName);
    	Assert.assertTrue(mod.behaviour instanceof OTPGenServerBehaviour);
    	Assert.assertTrue(mod.behaviour.dependencies.isEmpty());
    	
    	ErlangLabel initLabel = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'}", evalConf.config,evalConf.getLabelConverter())), 
    		labelLock = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}}", evalConf.config,evalConf.getLabelConverter())),
    		labelRead = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'call','read','AnyWibble'}", evalConf.config,evalConf.getLabelConverter())), 
    		labelWrite = mod.behaviour.convertErlToMod(AbstractLearnerGraph.generateNewLabel("{"+ErlangLabel.missingFunction+",'call',{'write','AnyWibble'},{'ok','AnyWibble'}}", evalConf.config,evalConf.getLabelConverter()));
    	mod.behaviour.getAlphabet().add(initLabel);
    	mod.behaviour.getAlphabet().add(labelLock);
    	mod.behaviour.getAlphabet().add(labelRead);
    	mod.behaviour.getAlphabet().add(labelWrite);
    	// Attempting first trace
    	List<Label> trace = Arrays.asList(new Label[]{initLabel,labelLock});
    	TraceOutcome tr = learner.askErlang(trace);
    	Assert.assertEquals(TRACEOUTCOME.TRACE_OK,tr.outcome);
    	Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'},{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}}]",RPNILearner.questionToString(Arrays.asList(tr.answerDetails)));
    	
    	tr = learner.askErlang(Arrays.asList(new Label[]{initLabel,labelLock, labelLock}));
    	
       	Assert.assertEquals(TRACEOUTCOME.TRACE_FAIL,tr.outcome);
    	Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'},{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}},{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}}]",RPNILearner.questionToString(Arrays.asList(tr.answerDetails)));
    	
    	tr =learner.askErlang(Arrays.asList(new Label[]{initLabel,labelLock,labelWrite, labelRead}));
    	
       	Assert.assertEquals(TRACEOUTCOME.TRACE_OK,tr.outcome);
    	Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'},{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}},{"+ErlangLabel.missingFunction+",'call',{'write','AnyWibble'},{'ok','AnyWibble'}},{"+ErlangLabel.missingFunction+",'call','read','AnyWibble'}]",
    			RPNILearner.questionToString(Arrays.asList(tr.answerDetails)));
    	
    	// Now attempt a "different output" input
    	ErlangLabel lbl = tr.answerDetails[3];
    	tr.answerDetails[3] = new ErlangLabel(lbl.function,lbl.callName,
    			lbl.input, new OtpErlangAtom("aa"));
    	mod.behaviour.getAlphabet().add(tr.answerDetails[3]);
    	tr =learner.askErlang(Arrays.asList(tr.answerDetails));
       	Assert.assertEquals(TRACEOUTCOME.TRACE_DIFFERENTOUTPUT,tr.outcome);
    	Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'init','AnyWibble','ok'},{"+ErlangLabel.missingFunction+",'call','lock',{'ok','locked'}},{"+ErlangLabel.missingFunction+",'call',{'write','AnyWibble'},{'ok','AnyWibble'}},{"+ErlangLabel.missingFunction+",'call','read','AnyWibble'}]",
    			RPNILearner.questionToString(Arrays.asList(tr.answerDetails)));
    }
    
	@Test
	public void testTraces() throws IOException
	{
		ErlangModule.setupErlangConfiguration(config,new File(ErlangExamples,"locker/locker.erl"));
		ErlangModule.loadModule(config);
		final String LBL1 = "{call, lock}", LBL2 = "{call, unlock}";
		final LearnerGraph gr = buildLearnerGraph("A- "+LBL1+" ->B-"+LBL2+"->B", "testConvertToModuleFailure1", config,null);
		Assert.assertEquals(2,gr.pathroutines.computeAlphabet().size());
		Iterator<Label> lblIter = gr.pathroutines.computeAlphabet().iterator();
		ErlangLabel lbl1 = (ErlangLabel)lblIter.next(), lbl2 = (ErlangLabel)lblIter.next();
		if (lbl1.input.toString().equals("unlock"))
		{// swap the elements 
			ErlangLabel tmp = lbl1;lbl1=lbl2;lbl2=tmp;
		}
		List<Label> trace = AbstractLearnerGraph.buildList(Arrays.asList(
				new String[]{LBL1,LBL2,LBL2}), config, null), expected = Arrays.asList(new Label[]{lbl1,lbl2,lbl2});
		Assert.assertEquals(expected,trace);
	}

	/** The name of test file - should not be static to ensure it picks the value of TestErlangRunner's variable
     * after it has been initialised.
     */
	protected String erlangFile = null, erlangFileOther = null;
    
    @Test
    public void testInvalidModuleName() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n");wr.close();
		File origFile = new File(erlangFile);
		// we are using timestamps to detect if a file needs recompiling. The test aims to check that renamed beam will fail to load; if the timestamp on the source is not earlier than that of the beam, the loader will attempt to recompile the source and also fail, largerly for the same reason although this is not what we are testing here.
		// We could place a sleep statement here, however this is not necessary because the process of going through to check if Erlang is still alive etc is enough to spend sufficient time to get a later timestamp. 
		ErlangRunner.compileErl(origFile, runner,false);
		final File renamedFile = new File(testDir.getAbsolutePath()+File.separator+"otherFile.erl");
		final Configuration configRenamed = config.copy();ErlangModule.setupErlangConfiguration(configRenamed,renamedFile);configRenamed.setErlangCompileIntoBeamDirectory(false);
		origFile.renameTo(new File(ErlangRunner.getName(renamedFile, ERL.ERL,false)));
		new File(ErlangRunner.getName(origFile,ERL.BEAM,false)).renameTo(new File(ErlangRunner.getName(renamedFile, ERL.BEAM,false)));
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(configRenamed);
		}},ErlangThrownException.class,"Invalid file name");// error message returned by Erlang code
    }

    @Test
    public void testLoadModule1() throws IOException
    {
    	File fileLocker = new File(ErlangExamples,"locker/locker.erl");
    	Assert.assertNull(ErlangModule.findModule("locker"));
    	ErlangModule.setupErlangConfiguration(config,fileLocker);
    	ErlangModule modA = ErlangModule.loadModule(config);
    	Assert.assertSame(modA,ErlangModule.findModule("locker"));
    }
    
    @Test
    public void testLoadModule2() throws IOException
    {
    	final File fileLocker = new File(ErlangExamples,"locker/locker.erl");
    	Assert.assertNull(ErlangModule.findModule("locker"));
   		ErlangModule.setupErlangConfiguration(config,fileLocker);
    	ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertSame(mod,ErlangModule.loadModule(config));
   }
    
    /** Forces the reload and checks that outcome is different every time. */
    @Test
    public void testLoadModule3() throws IOException
    {
    	final File fileLocker = new File(ErlangExamples,"locker/locker.erl");
    	Configuration loadConfiguration = config.copy();
    	loadConfiguration.setErlangSourceFile(fileLocker);
    	loadConfiguration.setErlangModuleName(fileLocker.getName());
    	loadConfiguration.setLabelKind(LABELKIND.LABEL_ERLANG);

    	Assert.assertNull(ErlangModule.findModule("locker"));
    	ErlangModule mod = ErlangModule.loadModule(loadConfiguration,true);
		Assert.assertNotSame(mod,ErlangModule.loadModule(loadConfiguration,true));
   }
    
    /** Tests that alphabet construction can be stopped from including module names in labels. */
    @Test
    public void testLoadModule4() throws IOException
    {
    	final File fileLocker = new File(ErlangExamples,"frequency/frequencyBroken.erl");
    	Configuration loadConfiguration = config.copy();
    	loadConfiguration.setErlangSourceFile(fileLocker);
    	loadConfiguration.setErlangModuleName(fileLocker.getName());
    	loadConfiguration.setLabelKind(LABELKIND.LABEL_ERLANG);

    	// First, load it with module names
    	ErlangModule mod = ErlangModule.loadModule(loadConfiguration,true);
    	Assert.assertTrue(TestTypes.getAlphabetAsString(mod).contains(mod.getName()) );
    	
    	// Now, load it without, the outcome should be unchanged because names are stripped by Synapse after learning
    	loadConfiguration.setErlangStripModuleNamesFromFunctionsInNonGenModules(true);
    	mod = ErlangModule.loadModule(loadConfiguration,true);
    	Assert.assertTrue(TestTypes.getAlphabetAsString(mod).contains(mod.getName()) );
    }
    
    protected static final String stdFunctions = "\nhandle_call(_,_,_)->{reply,ok,5}.\nhandle_cast(_,_)->{noreply,ok,5}.\nhandle_info(_,_)->{reply,ok}.\ninit(_)->{ok,5}.\n";
    
    @Test
    public void testExtraAttribute1() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n-justsomething(aa)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.ignoredBehaviours.isEmpty());
   }

    @Test
    public void testInvalidAttribute1() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n-behaviour(aa)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.ignoredBehaviours.contains("aa"));
   }

    @Test
    public void testInvalidAttribute2() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(56)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(config);
		}},IllegalArgumentException.class,"\"8\" is of the wrong kind");// 56 is interpreted as a string "8"
   }
    @Test
    public void testInvalidAttribute3() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(565)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(config);
		}},IllegalArgumentException.class,"565 is of the wrong type");
   }
    
    @Test
    public void testInvalidAttribute4() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(\"junk\")."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		checkForCorrectException(new whatToRun() { public @Override void run() throws IOException {
			ErlangModule.loadModule(config);
		}},IllegalArgumentException.class,"\"junk\" is of the wrong kind");
   }
    
    @Test
    public void testSpecificValue1() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		Assert.assertEquals("gen_server_wrapper",ErlangModule.loadModule(config).behaviour.getWrapperName());
   }
    
    @Test
    public void testSpecificValue2() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_fsm).\n"+
				"\nhandle_event(_,_,_)->{reply,ok,5}.\nhandle_sync_event(_,_)->{noreply,ok,5}.\n\ninit(_)->{ok,5}.\nhandle_info(_,_)->{reply,ok}.\n");wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		Assert.assertEquals("gen_fsm_wrapper",ErlangModule.loadModule(config).behaviour.getWrapperName());
   }
    
    /** One of the interface functions is missing. */ 
    @Test
    public void testSpecificValueFail() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server).\n"+
				"\nhandle_call(_,_,_)->{reply,ok,5}.\nhandle_cast(_,_)->{noreply,ok,5}.\n\ninitRenamed(_)->{ok,5}.\nhandle_info(_,_)->{reply,ok}.\n");wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'call','JustAnythingA','ok'}," +
				 "{"+ErlangLabel.missingFunction+",'call',[],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'call',['WibbleA'],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'call',['WibbleA','WobbleA'],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'cast','JustAnythingA','ok'}," +
				 "{"+ErlangLabel.missingFunction+",'cast',[],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'cast',['WibbleA'],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'cast',['WibbleA','WobbleA'],'ok'}," +
				 "{"+ErlangLabel.missingFunction+",'info','JustAnythingA',{'reply','ok'}}," +
				 "{"+ErlangLabel.missingFunction+",'info',[],{'reply','ok'}}," +
				 "{"+ErlangLabel.missingFunction+",'info',['WibbleA'],{'reply','ok'}}," +
				 "{"+ErlangLabel.missingFunction+",'info',['WibbleA','WobbleA'],{'reply','ok'}}]",
				 TestTypes.getAlphabetAsString(ErlangModule.loadModule(config)));
		
    }
    
    @Test
    public void testDependencies1() throws IOException
    {
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-behaviour(gen_server)."+stdFunctions);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		Assert.assertTrue(ErlangModule.loadModule(config).behaviour.dependencies.isEmpty());
   }
    
    @Test
    public void testDependencies2() throws IOException
    {
    	final String erlangFile2 = testDir.getAbsolutePath()+File.separator+"testFile2.erl";
		Writer wr = new FileWriter(erlangFile);wr.write("-module(testFile).\n-export[funct/1].\nfunct(5) -> ok.");wr.close();
		wr = new FileWriter(erlangFile2);wr.write("-module(testFile2).\n-export[f/1].\nf(2) -> testFile:funct(5).");wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
		Assert.assertTrue(ErlangModule.loadModule(config).behaviour.dependencies.isEmpty());
		Configuration cnf2 = config.copy();ErlangModule.setupErlangConfiguration(cnf2,new File(erlangFile2));
		Collection<String> deps = ErlangModule.loadModule(cnf2).behaviour.dependencies;
		Assert.assertEquals(1,deps.size());
		Assert.assertEquals("testFile",deps.toArray()[0]);
   }
    
    @Test
    public void testLoadExportsEmptySetBecauseFunctionDoesNotReturnAValue() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1]).\ntestFun([Arg])->io:format(\"42~n\"),halt().\n";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
  		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.behaviour.getAlphabet().isEmpty());
    }
    
    @Test
    public void testLoadExportsEmptySet1() throws IOException
    {
    	final String someErlang = "-module(testFile).\n\n";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.behaviour.getAlphabet().isEmpty());
    }
    
    @Test
    public void testLoadExportsEmptySet2() throws IOException
    {
    	final String someErlang = "-module(testFile).\ntestFun([Arg])->io:format(\"42~n\"),halt().\n";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.behaviour.getAlphabet().isEmpty());
    }

    @Test
    public void testLoadExports1() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1]).\ntestFun([Arg])->42.\n";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertEquals(
				"[{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[[]],42},"+
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble']],42}," +
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble','AnyListElemWibble']],42}" +
						"]",
						TestTypes.getAlphabetAsString(
				mod ));
	 }
    
    @Test
    public void testLoadExports2() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1]).\ntestFun([Arg])->42.\naFun(34)->33.";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[[]],42}," +
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble']],42}," +
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble','AnyListElemWibble']],42}" +
				"]"
				,TestTypes.getAlphabetAsString(mod ));
    }

    // Tests that where a function fails to terminate, it is not included in the list of those to attempt.
    @Test
    public void testLoadIgnoreFunctions() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1]).\ntestFun([Arg])->testFun([Arg]).\n";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertTrue(mod.behaviour.getAlphabet().isEmpty());
		Assert.assertTrue(mod.ignoredFunctions.contains("testFile:testFun/1"));
    }

    @Test
    public void testLoadExportsZeroArity1() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1]).\ntestFun([Arg])->42.\naFun()->33.";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		config.setErlangAlphabetAnyListLength(1);
   		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[[]],42},"+
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble']],42}"+
				"]",TestTypes.getAlphabetAsString(mod ));
    }

    @Test
    public void testLoadExportsZeroArity2() throws IOException
    {
    	final String someErlang = "-module(testFile).\n-export([testFun/1,aFun/0]).\ntestFun([Arg])->42.\naFun()->33.";
   		Writer wr = new FileWriter(erlangFile);wr.write(someErlang);wr.close();
   		ErlangModule.setupErlangConfiguration(config,new File(erlangFile));
   		config.setErlangAlphabetAnyListLength(1);
  		ErlangModule mod = ErlangModule.loadModule(config);
		Assert.assertEquals("[{"+ErlangLabel.missingFunction+",'testFile:aFun/0',[],33},"+
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[[]],42},"+
				"{"+ErlangLabel.missingFunction+",'testFile:testFun/1',[['AnyListElemWibble']],42}"+
				"]",TestTypes.getAlphabetAsString(mod));
    }
}
