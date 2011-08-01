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
package statechum.apps;

import java.awt.Frame;
import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Label;
import statechum.Pair;
import statechum.PrefixTraceTree;
import statechum.Trace;
import statechum.analysis.CodeCoverage.CodeCoverageMap;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.Erlang.ErlangModule;
import statechum.analysis.Erlang.OTPBehaviour;
import statechum.analysis.Erlang.OTPBehaviour.ConvertALabel;

import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 *
 * @author ramsay
 */
/**
 * Extends QSMTool to use an Erlang oracle to answer the questions...
 * 
 * @author ramsay
 */
public class ErlangQSMOracle {

	// public static String erlangModule;
	public static Collection<String> erlangModules;
	// public static String erlangWrapperModule;
	public static String covermapFile;
	public static String ErlangFolder = "ErlangOracle";
	public static String ErlangTyper = "lib/modified_typer";

	// Mode can be "basic" or "otp". OTP will use the OTP wrappers to infer
	// stuff about an OTP behaviour module
	public static String mode = "basic";
	public static String initArgs;
	public static PrefixTraceTree ErlangTraces;

	// How many times should we try to expand the graph by extending the deepest
	// node with the entire alphabet?
	public static int exhaustTries = 0;

	// This map stores coverage maps in the form (Prefix, Suffix) -> Coverage
	// i.e. the coverage map calculated from the end of trace Prefix to the end
	// of state Suffix
	// The Map is indexed by the string representation of the prefix and suffix
	// separated by a '-', in Erlang form
	// e.g. "[]-[a,b,c]" or "[a,b]-[a,b,c]"
	public static Map<Pair<Trace, Trace>, CodeCoverageMap> coverageMaps = new TreeMap<Pair<Trace, Trace>, CodeCoverageMap>();
	public static boolean coverageMapLock = false;

	public static void main(String[] args) 
	{
		startInference(args[0]);
	}

	/** Given a bunch of traces with potentially missing function information, this one converts
	 * all traces to those containing that information.
	 * 
	 * @param traces traces to convert
	 * @return outcome of conversion
	 */
	public static Set<List<Label>> convertTracesToErl(Set<List<Label>> traces, Configuration config)
	{
		ErlangModule mod = ErlangModule.findModule(config.getErlangModuleName());
		Set<List<Label>> convertedTraces = null;
		ConvertALabel converter = mod.behaviour.new ConverterErlToMod();
		convertedTraces = new HashSet<List<Label>>();
		for(List<Label> list:traces) convertedTraces.add(OTPBehaviour.convertTrace(list, converter));
		return convertedTraces;
	}
	
	/** Creates and initialises a learner. 
	 * @param parentFrame the frame relative to which questions are to be displayed. Can be null.
	 * @param tracesFile the name of the file containing traces and learner configuration.
	 * @return Learner ready to learn upon call to <em>learnMachine()</em>.
	 */
	public static ErlangOracleLearner createLearner(Frame parentFrame, String tracesFile)
	{
		QSMTool tool = new QSMTool();
		tool.loadConfig(tracesFile);

		QSMTool.setSimpleConfiguration(tool.learnerInitConfiguration.config,true, 0);

		// This one actually loads a module.
		ErlangOracleLearner innerLearner = new ErlangOracleLearner(parentFrame,	tool.learnerInitConfiguration);
		
		Set<List<Label>> 
			positives = convertTracesToErl(tool.sPlus,tool.learnerInitConfiguration.config),
			negatives = convertTracesToErl(tool.sMinus,tool.learnerInitConfiguration.config);
		Set<ErlangLabel> alphabet = ErlangModule.findModule(tool.learnerInitConfiguration.config.getErlangModuleName()).behaviour.getAlphabet();
		for(List<Label> list:positives) for(Label lbl:list) alphabet.add((ErlangLabel)lbl);
		for(List<Label> list:negatives) for(Label lbl:list) alphabet.add((ErlangLabel)lbl);

		innerLearner.initInputToPossibleOutputsMap();
		innerLearner.init(positives, negatives);
		return innerLearner;
	}

	/** Infers an automaton from the supplied input file.
	 * @param tracesFile the name of the file containing traces and learner configuration.
	 */
	public static LearnerGraph startInference(String tracesFile) 
	{

		ErlangOracleVisualiser viz = new ErlangOracleVisualiser();
		ErlangOracleLearner innerLearner = createLearner(viz,tracesFile);
		innerLearner.addObserver(viz);
		return innerLearner.learnMachine();
	}

	protected static Collection<Label> getPathTo(CmpVertex tgt, CmpVertex root,
			Map<CmpVertex, Map<Label, CmpVertex>> transitionMatrix,
			Collection<CmpVertex> seenStates) {
		Map<Label, CmpVertex> trans = transitionMatrix.get(root);
		for (Label s : trans.keySet()) {
			CmpVertex dest = trans.get(s);
			if (dest == tgt) {
				// A hit, a hit, a very palpable hit...
				ArrayList<Label> result = new ArrayList<Label>();
				result.add(s);
				return result;
			} else {
				// Maybe a recursive hit?...
				// Cycles would be bad :)
				if (!seenStates.contains(dest)) {
					ArrayList<CmpVertex> newSeen = new ArrayList<CmpVertex>(
							seenStates);
					newSeen.add(dest);
					Collection<Label> subpath = getPathTo(tgt, dest,
							transitionMatrix, newSeen);
					if (subpath != null) {
						ArrayList<Label> result = new ArrayList<Label>();
						result.add(s);
						result.addAll(subpath);
						return result;
					}
				}
			}
		}
		// Not found -- null return...
		return null;
	}

	protected static void wildCardStrip(String filename) {
		// System.out.println("Stripping wildcards from " + filename);
		ArrayList<String> lines = new ArrayList<String>();
		BufferedReader input = null;
		try {
			input = new BufferedReader(new FileReader(filename));
			String line;
			while ((line = input.readLine()) != null) {
				if (line.indexOf("'*'") < 0) {
					lines.add(line);
					// System.out.println("Keeping " + line);
				} else {
					// System.out.println("Stripping " + line);
				}
			}
			input.close();
			(new File(filename)).delete();
			BufferedWriter out = new BufferedWriter(new FileWriter(filename));
			for (String l : lines) {
				out.write(l);
				out.newLine();
			}
			out.flush();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/*
	 * NO LONGER USED public static void createInitTraces() throws IOException {
	 * String erlArgs; erlArgs = "tracer2:gen_random_traces(" +
	 * erlangWrapperModule + "," + erlangModule + "," + initArgs + "," +
	 * erlangAlphabet + ",\"" + tracesFile + "\"," +
	 * ErlangOracleVisualiser.toErlangList(erlangModules) + ")"; // erlArgs =
	 * "tracer2:gen_exhaust_traces(" + erlangWrapperModule + "," + //
	 * erlangModule + "," + initArgs + "," + erlangAlphabet + ",\"" + //
	 * tracesFile + "\"," + //
	 * ErlangOracleVisualiser.toErlangList(erlangModules) + ")";
	 * 
	 * System.out.println("Evaluating " + erlArgs + " in folder " +
	 * ErlangFolder); // ./erlinittraces.sh testmod1 testfun
	 * [1,4,8,16,32,37,41,42] test2.out // [testmod1,testmod2] in folder
	 * ErlangOracle // ErlangOracleLearner.runErlang(erlArgs); }
	 */

	public static void loadCoverageMaps() {
		coverageMaps = new TreeMap<Pair<Trace, Trace>, CodeCoverageMap>();
		loadCoverageMaps(ErlangFolder + "/" + covermapFile);
	}

	public static void loadCoverageMaps(String filename) {
		while (coverageMapLock) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				;
			}
		}
		coverageMapLock = true;
		// System.out.println("Loading coverage maps from " + filename + "...");
		BufferedReader input = null;
		try {
			input = new BufferedReader(new FileReader(filename));
			String line;
			while ((line = input.readLine()) != null) {
				// This assumes a format of [Trace] => [Coverage map]
				String[] toks = line.split("=>");

				Pair<Trace, Trace> index = new Pair<Trace, Trace>(new Trace(),
						Trace.fromString(toks[0].trim()));
				String map = toks[1].trim();
				map = map.substring(1, map.length() - 1);

				CodeCoverageMap mapObject = new CodeCoverageMap();
				if (map.length() > 0) {
					// Create the parsed coverage map object
					String[] maplets = (map.trim()).split("\\},\\{");
					// Trim the {} off the first and last items...
					maplets[0] = maplets[0].substring(1);
					maplets[maplets.length - 1] = maplets[maplets.length - 1]
							.substring(0,
									maplets[maplets.length - 1].length() - 1);
					for (String m : maplets) {
						// Maplets have the form {line, count} but should be
						// missing the {} from the way we split the string.
						String[] parts = m.split(",");
						mapObject.add(parts[0], Integer.parseInt(parts[1]));
					}

				}
				coverageMaps.put(index, mapObject);
				// System.out.println("Loading coverage map for \"" +
				// index.secondElem + "\" = " + coverageMaps.get(index) + " (" +
				// (coverageMaps.get(index) == null) + ")");
			}
			// System.out.println("Coverage maps:\n" + coverageMaps.toString());
		} catch (FileNotFoundException e) {
			System.out.println("Couldn't open coverage map file " + filename);
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (input != null) {
				try {
					input.close();
				} catch (IOException e) { /* ignore this */
				}
			}
		}
		coverageMapLock = false;
	}
}
