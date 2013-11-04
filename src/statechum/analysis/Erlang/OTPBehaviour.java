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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.Configuration;
import statechum.Label;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.Erlang.Signatures.Signature;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;
import statechum.analysis.learning.rpnicore.Transform.LabelConverter;

/**
 * 
 * @author ramsay
 */
public abstract class OTPBehaviour {

	public String name;
	final protected ErlangModule parent;

	public static interface OtpCallInterface {
		/**
		 * Otp returns the middle-arg of methods which expect to return a value.
		 */
		public Signature extractVisibleReturnType(Signature fullReturnType);

		/**
		 * In OTP, only the first argument is supplied by as, the rest are by
		 * framework.
		 * 
		 * @param args
		 *            types of args to extract the first element from
		 * @return outcome of conversion
		 */
		public List<List<Signature>> convertArguments(List<List<Signature>> args);

		/** Arity of the operation. */
		public int getArity();

		/** Returns the corresponding Otp name. */
		public String getOtpName();
	}

	/** Determines the conversion to get from a normal function to OTP one. */
	public static class OTPCall implements OtpCallInterface {
		/**
		 * Otp returns the middle-arg of methods which expect to return a value.
		 */
		@Override
		public Signature extractVisibleReturnType(Signature fullReturnType) {
			return Signature.extractElement(fullReturnType, 1);
		}

		/**
		 * In OTP, only the first argument is supplied by as, the rest are by
		 * framework.
		 * 
		 * @param args
		 *            types of args to extract the first element from
		 * @return outcome of conversion
		 */
		@Override
		public List<List<Signature>> convertArguments(List<List<Signature>> args) {
			List<List<Signature>> outcome = new LinkedList<List<Signature>>();
			for (List<Signature> list : args) {
				List<Signature> convertedList = new LinkedList<Signature>();
				convertedList.add(list.get(0));
				outcome.add(convertedList);
			}
			return Collections.unmodifiableList(outcome);

		}

		@Override
		public int getArity() {
			return 1;
		}

		@Override
		public String getOtpName() {
			return "call";
		}
	}

	protected final Map<String, OtpCallInterface> patterns;
	protected Set<ErlangLabel> alphabet;
	
	/** Lists names of the modules the current module is dependent on. */
	public Collection<String> dependencies;
	protected static final String[] stdmodsarray = { "erlang", "gen_server", "gen_fsm", "lists",
			"supervisor", "filename", "io", "io_lib", "code", "erl_ddll", "application", "math" };
	protected static final ArrayList<String> stdModsList = new ArrayList<String>(Arrays.asList(stdmodsarray));

	protected OTPBehaviour(ErlangModule mod) {
		name = null;
		parent = mod;
		alphabet = new LinkedHashSet<ErlangLabel>();
		patterns = new TreeMap<String, OtpCallInterface>();
		dependencies = new LinkedList<String>();
	}

	/**
	 * We have normal OTP functions but all communications is via the server
	 * interface, hence we create functions reflecting what we actually see
	 * during learning.<br/>
	 * The other case is when we do not actually talk to the server, in which
	 * case the contents of sigs (except for module_info) should be added to the
	 * alphabet.
	 */
	protected void generateAlphabet(Configuration config) {
		if (patterns.isEmpty()) {
			Set<String> exports = loadExports(config);
			for (Entry<String, FuncSignature> sigEntry : parent.sigs.entrySet()) {
				if (!sigEntry.getValue().getName().equals("module_info")
						&& exports.contains(sigEntry.getKey()))
					addFunctionToAlphabet(sigEntry.getKey(), sigEntry.getValue(), config);
			}
		} else
			for (Entry<String, OtpCallInterface> pattern : patterns.entrySet()) {
				if (!parent.sigs.containsKey(pattern.getKey())) {
					// Really, I don't care...
					// throw new
					// IllegalArgumentException("function "+pattern.getKey()+" is missing in module "+parent.getName());
					System.out.println("function " + pattern.getKey() + " is missing in module " + parent.getName());
				} else {
					String otpName = pattern.getValue().getOtpName();
					if (parent.sigs.containsKey(otpName))
						throw new IllegalArgumentException("there is already a function defined with name "
								+ otpName + " in module " + parent.getName());
					FuncSignature 
						origFunction = parent.sigs.get(pattern.getKey()), 
						otpFunction = new FuncSignature(config, ErlangLabel.parseText(origFunction.toErlangTerm()), 
								pattern.getValue());

					parent.sigs.put(otpName, otpFunction);
					addFunctionToAlphabet(otpName, otpFunction, config);
				}
			}
	}

	/**
	 * Used to take an existing function and generate an i/o pair for inclusion
	 * in an alphabet.
	 * 
	 * @param callName
	 *            how the function should be called in traces. Important for OTP
	 *            functions but should be the same as function name for ordinary
	 *            exported functions.
	 * @param function
	 *            function to be associated with this i/o pair.
	 * @param config
	 *            determines whether outputs are to be ignored.
	 */
	abstract void addFunctionToAlphabet(String callName, FuncSignature function, Configuration config);

	@Override
	public String toString() {
		return name;
	}

	public String getWrapperName() {
		return name + "_wrapper";
	}

	public Collection<String> getDependencies() {
		return dependencies;
	}

	/**
	 * Extracts dependencies of the supplied module, assuming the module has
	 * been successfully compiled and .beam file exists.
	 * 
	 * @param file
	 *            the file of the module
	 * @throws IOException
	 *             if this fails.
	 */
	public void loadDependencies(File file, Configuration config) {
		OtpErlangTuple response = ErlangRunner.getRunner(config.getErlangMboxName()).call(
				new OtpErlangObject[] { new OtpErlangAtom("dependencies"),
						new OtpErlangAtom(ErlangRunner.getName(file, ErlangRunner.ERL.BEAM, config.getErlangCompileIntoBeamDirectory())) },
				"Could not load dependencies of " + file.getName());
		
		// the first element is 'ok'
		OtpErlangList listOfDepTuples = (OtpErlangList) response.elementAt(1);
		for (OtpErlangObject tup : listOfDepTuples.elements()) {
			String mod = ((OtpErlangAtom) ((OtpErlangTuple) tup).elementAt(0)).atomValue();
			if (!stdModsList.contains(mod) && !dependencies.contains(mod)) {
				dependencies.add(mod);
			}
		}
	}

	/**
	 * Returns a list of fully-qualified names of functions which have been
	 * exported from this module.
	 */
	public Set<String> loadExports(Configuration config) {
		Set<String> result = new TreeSet<String>();
		OtpErlangTuple response = ErlangRunner.getRunner(config.getErlangMboxName()).call(
				new OtpErlangObject[] {
						new OtpErlangAtom("exports"),
						new OtpErlangAtom(ErlangRunner.getName(new File(parent.sourceFolder, parent.getName()
								+ ERL.ERL), ErlangRunner.ERL.BEAM,config.getErlangCompileIntoBeamDirectory())) },
				"Could not load exports of " + parent.getName());

		OtpErlangList listOfExportTuples = (OtpErlangList) response.elementAt(1);// the first element is 'ok'
		for (OtpErlangObject tup : listOfExportTuples.elements()) {
			String funName = ((OtpErlangAtom) ((OtpErlangTuple) tup).elementAt(0)).atomValue();
			if (!funName.equals("module_info")) 
			{
				long funArity = ((OtpErlangLong) ((OtpErlangTuple) tup).elementAt(1)).longValue();
				String qualifiedName = FuncSignature.qualifiedNameFromFunction(parent.getName(), funName,funArity);
				if (!parent.ignoredFunctions.contains(qualifiedName))
				{
					assert parent.sigs.containsKey(qualifiedName);
					result.add(qualifiedName);
				}
			}
		}

		return result;
	}

	public static OTPBehaviour obtainDeclaredBehaviour(File file, Configuration config, ErlangModule mod,Collection<String> ignoredBehaviours) {
		OTPBehaviour behaviour = new OTPUnknownBehaviour(mod);// unknown unless defined in a module
		// extract the list of attributes and determine the kind of this module
		OtpErlangTuple response = ErlangRunner.getRunner(config.getErlangMboxName()).call(
				new OtpErlangObject[] { new OtpErlangAtom("attributes"),
						new OtpErlangAtom(ErlangRunner.getName(file, ErlangRunner.ERL.BEAM,config.getErlangCompileIntoBeamDirectory())) },
				"Could not load attributes of " + file.getName());

		OtpErlangList listOfDepTuples = (OtpErlangList) response.elementAt(1);// the first element is 'ok'
		for (OtpErlangObject tup : listOfDepTuples.elements()) {
			OtpErlangTuple tuple = (OtpErlangTuple) tup;
			OtpErlangObject name = tuple.elementAt(0);
			if (name instanceof OtpErlangAtom && ((OtpErlangAtom) name).atomValue().equals("behaviour")) {// found the OTP behaviour attribute
				OtpErlangObject value = tuple.elementAt(1);
				if (value instanceof OtpErlangList) // list of behaviours
				{
					OtpErlangList behList = (OtpErlangList)value;
					for(int i=0;i<behList.arity();++i)
						if (behList.elementAt(i) instanceof OtpErlangAtom)
						{
							String bstring = ((OtpErlangAtom) (behList.elementAt(i))).atomValue();
							if (bstring.startsWith("gen_server")) {
								behaviour = new OTPGenServerBehaviour(mod);
							} else if (bstring.startsWith("gen_event")) {
								behaviour = new OTPGenEventBehaviour(mod,config);
							} else if (bstring.startsWith("gen_fsm")) {
								behaviour = new OTPGenFSMBehaviour(mod,config);
							}
							else
							{
								ignoredBehaviours.add(bstring);
								System.out.println("Warning: unknown behaviour "+bstring);
							}
						}
						else
							throw new IllegalArgumentException("behaviour attribute " + behList.elementAt(i)
									+ " is of the wrong type");
							
					
				} else
					throw new IllegalArgumentException("behaviour attribute " + value
							+ " is of the wrong kind");
			}
		}

		behaviour.loadDependencies(file,config);
		return behaviour;
	}

	public Set<ErlangLabel> getAlphabet() {
		return alphabet;
	}
	
	public void addToAlphabet(ErlangLabel lbl)
	{
		if (alphabet.contains(lbl))
			throw new IllegalArgumentException("Label "+lbl+" is already in the alphabet, cannot add");
		alphabet.add(lbl);
	}

	/**
	 * Given an instance of a converter, converts all elements of a trace. This
	 * is useful for converting from text-compatible traces (no function
	 * signatures) to those with signatures and back.
	 * 
	 * @param trace
	 *            trace to convert
	 * @param converter
	 *            how to convert, see <em>ConverterModToErl</em> and
	 *            <em>ConverterErlToMod</em>.
	 * @return result of conversion.
	 */
	public static List<Label> convertTrace(List<Label> trace, ConvertALabel converter) {
		List<Label> outcome = new LinkedList<Label>();
		for (Label lbl : trace)
			outcome.add(converter.convertLabelToLabel(lbl));
		return outcome;
	}

	/**
	 * Used to turn real traces into textual traces, suitable for persistence.
	 */
	public class ConverterModToErl implements LabelConverter, ConvertALabel {
		@Override
		public Set<Label> convertLabel(Label lbl) {
			return Collections.singleton(convertLabelToLabel(lbl));
		}

		@Override
		public Label convertLabelToLabel(Label lbl) {
			return convertModToErl(lbl);
		}
	}

	/**
	 * Used to turn textual traces loaded from somewhere into proper Erlang
	 * traces which can be executed.
	 */
	public class ConverterErlToMod implements LabelConverter, ConvertALabel {
		@Override
		public Set<Label> convertLabel(Label lbl) {
			return Collections.singleton(convertLabelToLabel(lbl));
		}

		@Override
		public Label convertLabelToLabel(Label lbl) {
			return convertErlToMod(lbl);
		}

	}

	/** Strips the description of the function from the supplied label. */
	public static Label convertModToErl(Label lbl) {
		if (!(lbl instanceof ErlangLabel))
			throw new IllegalArgumentException("cannot convert non-erlang labels");
		ErlangLabel label = (ErlangLabel) lbl;

		return new ErlangLabel(null, label.callName, label.input, label.expectedOutput);
	}

	/**
	 * Using the behaviour, identify the function corresponding to the name of
	 * label and creates a new label with that function.
	 * 
	 * @param lbl
	 *            label to convert
	 * @return result of conversion.
	 */
	public ErlangLabel convertErlToMod(Label lbl) {
		if (!(lbl instanceof ErlangLabel))
			throw new IllegalArgumentException("cannot convert non-erlang labels");
		ErlangLabel label = (ErlangLabel) lbl;

		FuncSignature origFunc = parent.sigs.get(label.callName);
		if (origFunc == null)
			throw new IllegalArgumentException("unknown function \"" + label.callName + "\" in module "
					+ parent.getName());

		// At this point, we know which function should correspond to this label,
		// it is worth checking whether the function already associated with the label
		// is the correct function,
		if (label.function != null) {
			if (!label.function.toErlangTerm().equals(origFunc.toErlangTerm()))
				throw new IllegalArgumentException(
						"label already has a function assigned and it is a different function, " + "was : "
								+ label.function + ", now: " + origFunc);
		}
		return new ErlangLabel(origFunc, label.callName, label.input, label.expectedOutput);
	}

	/**
	 * In an ordinary function called via <em>apply</em> or so, arguments are
	 * supplied in the form of a list. For Otp functions this is not the case -
	 * functions take a single argument. In order to moderate between the two, a
	 * conversion function is introduced which takes an Otp argument and turns
	 * it into pure Erlang one.
	 * 
	 * For an Unknown behaviour this should assert that an arg is a list and
	 * convert it into Java list, for Otp behaviours, we make singleton lists
	 * because all function take single arguments.
	 */
	public abstract List<OtpErlangObject> functionArgumentsToListOfArgs(OtpErlangObject arg);

	public List<List<OtpErlangObject>> getInitArgs() {
		FuncSignature init = parent.sigs.get(parent.getName() + ":init/1");
		if(init != null)
			return init.instantiateAllArgs();

		return new LinkedList<List<OtpErlangObject>>();
	}
}
