/* Copyright (c) 2011 The University of Sheffield.
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
 * 
 */
package statechum.analysis.Erlang;

import statechum.Configuration;
import statechum.Configuration.LABELKIND;
import statechum.Helper;
import statechum.ProgressIndicator;
import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.Erlang.ErlangRunner.ErlangThrownException;
import statechum.analysis.Erlang.Signatures.FuncSignature;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.Assert;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Represents an Erlang module
 * 
 * @author ramsay
 */
public class ErlangModule {

	public final File sourceFolder;
	public final String name;
	public OTPBehaviour behaviour;
	public final Map<String, FuncSignature> sigs;
	public final Map<String, OtpErlangTuple> sigTypes;
	public final Set<String> ignoredFunctions, ignoredBehaviours;
	
	private ErlangModule(Configuration config) throws IOException 
	{
		final File f = config.getErlangSourceFile();
		name = ErlangRunner.getName(f, ERL.MOD,config.getErlangCompileIntoBeamDirectory());
		sourceFolder = f.getParentFile();
		ProgressIndicator progress = new ProgressIndicator(name, 7);
		// launch Erlang by calling a test method.
		ErlangRunner erlangRunner = ErlangRunner.getRunner(config.getErlangMboxName()); 
		erlangRunner.call(
				new OtpErlangObject[] { new OtpErlangAtom("echo2Tuple"),
						new OtpErlangAtom("aaa") }, "echo2Tuple");
		progress.next();// 1

		// Compile and typecheck the module...
		ErlangRunner.compileErl(f, erlangRunner,config.getErlangCompileIntoBeamDirectory());
		progress.next();// 2
		sigs = new TreeMap<String, FuncSignature>();ignoredFunctions = new TreeSet<String>();ignoredBehaviours = new TreeSet<String>();

		File pltFile = new File(ErlangRunner.getName(f, ERL.PLT,config.getErlangCompileIntoBeamDirectory()));

		// Almost the same arguments for dialyzer and typer, the first argument
		// determines which of the two to run.
		OtpErlangObject[] otpArgs = new OtpErlangObject[] {
				null, // either Dialyzer or typer

				new OtpErlangList(new OtpErlangObject[] { new OtpErlangString(
						ErlangRunner.getName(f, ERL.BEAM,config.getErlangCompileIntoBeamDirectory())) }),
				new OtpErlangString(ErlangRunner.getName(f, ERL.PLT,config.getErlangCompileIntoBeamDirectory())),
				new OtpErlangList(new OtpErlangObject[] { new OtpErlangString(
						ErlangRunner.getName(f, ERL.ERL,false))}) };

		if (!pltFile.canRead() || f.lastModified() > pltFile.lastModified()) {// rebuild the PLT file since the source was modified or the plt file does not exist
			//noinspection ResultOfMethodCallIgnored
			pltFile.delete();
			otpArgs[0] = new OtpErlangAtom("dialyzer");
			erlangRunner.call(otpArgs, "Could not run dialyzer");
		}
		progress.next();// 3

		// Typer always has to be run
		otpArgs[0] = new OtpErlangAtom("typer");
		OtpErlangTuple response = null;
		try
		{
			response = erlangRunner.call(otpArgs,"Could not run typer");
			
			progress.next();// 4
			progress.next();// 5
		}
		catch(ErlangThrownException ex)
		{
			//noinspection ResultOfMethodCallIgnored
			pltFile.delete();
			otpArgs[0] = new OtpErlangAtom("dialyzer");
			progress.next();// 4
			erlangRunner.call(otpArgs, "Could not run dialyzer");
			otpArgs[0] = new OtpErlangAtom("typer");
			progress.next();// 5
			response = erlangRunner.call(otpArgs,"Could not run typer for the second time");			
		}
		progress.next();// 6

		sigTypes = new TreeMap<String,OtpErlangTuple>();
		OtpErlangList analysisResults = (OtpErlangList) response.elementAt(1);
		Assert.assertEquals(1, analysisResults.arity());
		OtpErlangTuple fileDetails = (OtpErlangTuple) analysisResults.elementAt(0);
		OtpErlangList typeInformation = (OtpErlangList) fileDetails.elementAt(2);
		for (int i = 0; i < typeInformation.arity(); ++i) {
			OtpErlangTuple functionDescr = (OtpErlangTuple) typeInformation.elementAt(i); 
			if (functionDescr.arity() > 3)
			{
				FuncSignature s = new FuncSignature(config,functionDescr, null);
				sigTypes.put(s.getQualifiedName(), functionDescr);
				//sigs.put(s.getQualifiedName(), s);
			}
			else
			{// if not a function signature, it is an error message. The first two elements are function name and arity, we add this module name.
				String fullName = FuncSignature.qualifiedNameFromFunction(getName(),
						((OtpErlangAtom)functionDescr.elementAt(0)).atomValue(),
						((OtpErlangLong)functionDescr.elementAt(1)).longValue());
				ignoredFunctions.add( fullName );
				System.out.println("Ignoring: "+fullName+", "+functionDescr.elementAt(2));
			}
		}
		rebuildSigs(config, Collections.emptyMap());
		progress.next();// 7
	}

	/** Sometimes we might want to override type definitions provided by typer. This function permits one to construct alphabet based on updated types. 
	 * @param updatesToTypes overrides to type information 
	 */
	public void rebuildSigs(Configuration config,Map<String, OtpErlangTuple> updatesToTypes)
	{
		sigTypes.putAll(updatesToTypes);sigs.clear();
		for(Entry<String,OtpErlangTuple> entry:sigTypes.entrySet())
		{
			OtpErlangTuple functionDescr = entry.getValue();
			if (functionDescr.arity() > 3)
			{
				FuncSignature s = new FuncSignature(config,functionDescr, null);
				if (!s.getQualifiedName().equals(entry.getKey()))
					throw new IllegalArgumentException("invalid override, expected name "+entry.getKey()+", got "+s.getQualifiedName());
				sigs.put(entry.getKey(), s);
			}
			else
				throw new IllegalArgumentException("invalid type of an Erlang function");
		}	
		behaviour = OTPBehaviour.obtainDeclaredBehaviour(config.getErlangSourceFile(), config, this,ignoredBehaviours);
	}

	public String getName() {
		assert name != null;
		return name;
	}

	@Override
	public String toString() {
		return getName() + " [" + behaviour.toString() + "] ("
				+ behaviour.dependencies.size() + " dependencies)";
	}

	protected final static Map<String, ErlangModule> modulesRegistry = new TreeMap<String, ErlangModule>();


	public static ErlangModule loadModule(String moduleName) throws IOException {
		Configuration config = Configuration.getDefaultConfiguration().copy();setupErlangConfiguration(config, new File(moduleName));
		return loadModule(config);
	}
	
	/** Updates the supplied configuration with values necessary to load a supplied module. */
	public static void setupErlangConfiguration(Configuration config, File module)
	{
		config.setErlangSourceFile(module);
		config.setErlangModuleName(ErlangRunner.getName(module, ERL.MOD, config.getErlangCompileIntoBeamDirectory()));
    	config.setLabelKind(LABELKIND.LABEL_ERLANG);
	}
		
	public static ErlangModule loadModule(Configuration config) throws IOException {
		return loadModule(config, false);
	}

	public static ErlangModule loadModule(Configuration config, boolean forceReload) throws IOException {
		ErlangModule mod = new ErlangModule(config);
		if ((!modulesRegistry.containsKey(mod.getName()))||forceReload) {
			modulesRegistry.put(mod.getName(), mod);
			mod.behaviour.generateAlphabet(config);
		} else {
			mod = modulesRegistry.get(mod.getName());
		}
		return mod;
	}

	/**
	 * Finds the respective module - it cannot load one because this is usually
	 * called using module name from a configuration which does not have a
	 * corresponding file name.
	 */
	public static ErlangModule findModule(String erlangModuleName) {
		ErlangModule result = null;
		if (erlangModuleName != null)
			result = modulesRegistry.get(erlangModuleName);
		return result;
	}

	/**
	 * This method is intended for testing - it throws away any loaded modules,
	 * hence permitting me to regenerate a module and load the new one.
	 */
	public static void flushRegistry() {
		modulesRegistry.clear();
	}
}
