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

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;
import statechum.DeterministicDirectedSparseGraph.CmpVertex;
import statechum.Helper;
import statechum.Label;
import statechum.Pair;
import statechum.PrefixTraceTree;
import statechum.Trace;
import statechum.analysis.CodeCoverage.CodeCoverageMap;
import statechum.analysis.Erlang.ErlangLabel;

import statechum.analysis.learning.*;
import statechum.analysis.learning.rpnicore.LearnerGraph;

/**
 *
 * @author ramsay
 */
/** Extends QSMTool to use an Erlang oracle to answer the questions...
 *
 * @author ramsay
 */
public class ErlangQSMOracle {

    public static String erlangModule;
    public static Collection<String> erlangModules;
    public static String erlangWrapperModule;
    public static String erlangAlphabet;
    public static Collection<ErlangLabel> moduleAlphabet;
    public static String tracesFile;
    public static String covermapFile;
    public static String ErlangFolder = "ErlangOracle";
    public static String ErlangTyper = "lib/modified_typer";
    
    // Mode can be "basic" or "otp". OTP will use the OTP wrappers to infer stuff about an OTP behaviour module
    public static String mode = "basic";
    public static String initArgs;
    public static PrefixTraceTree ErlangTraces;

    // How many times should we try to expand the graph by extending the deepest node with the entire alphabet?
    public static int exhaustTries = 0;

    // This map stores coverage maps in the form (Prefix, Suffix) -> Coverage
    // i.e. the coverage map calculated from the end of trace Prefix to the end of state Suffix
    // The Map is indexed by the string representation of the prefix and suffix separated by a '-', in Erlang form
    // e.g. "[]-[a,b,c]" or "[a,b]-[a,b,c]"
    public static Map<Pair<Trace, Trace>, CodeCoverageMap> coverageMaps = new TreeMap<Pair<Trace, Trace>, CodeCoverageMap>();
    public static boolean coverageMapLock = false;

    public static void main(String[] args) {
        // Generate some basic traces to get QSM started
        erlangModule = args[1];
        erlangWrapperModule = args[2];
        tracesFile = args[0];
        covermapFile = tracesFile + ".covermap";
        erlangAlphabet = args[3];

        erlangModules = new LinkedList<String>();
        //erlangModules.add(erlangModule);
        for (int i = 4; i < args.length; i++) {
            erlangModules.add(args[i]);
        }
        try {
            startInference();
        } catch (IOException e) {
            Helper.throwUnchecked("failed to generate random traces", e);
        }

    }

    public static LearnerGraph startInference() throws IOException {
        // Clear the files...
        //(new File(ErlangFolder, tracesFile)).delete();
        //(new File(ErlangFolder, covermapFile)).delete();
        //createInitTraces();
        //loadCoverageMaps();

        ErlangTraces = new PrefixTraceTree(ErlangFolder + File.separator + tracesFile);
        //System.out.println("Traces Tree:\n" + ErlangTraces.toString());

        // Strip wildcard traces from the file...
        wildCardStrip(ErlangFolder + File.separator + tracesFile);

        QSMTool tool = new QSMTool();
        tool.loadConfig(ErlangFolder + File.separator + tracesFile);

        QSMTool.setSimpleConfiguration(tool.learnerInitConfiguration.config, true, 0);
        ErlangOracleVisualiser viz = new ErlangOracleVisualiser();
        // This is the one line thats actually changed...
        ErlangOracleLearner innerLearner = new ErlangOracleLearner(viz, tool.learnerInitConfiguration,null);
        innerLearner.addObserver(viz);
        LearnerGraph graph = innerLearner.learnMachine(tool.sPlus, tool.sMinus);
        boolean complete = false;
        int repeats = 0;
        while ((graph != null) && (!complete) && (repeats < exhaustTries)) {
            repeats++;
            Map<CmpVertex, Map<Label, CmpVertex>> transitionMatrix = graph.getTransitionMatrix();
            // Find (one of) the deepest node(s)
            CmpVertex deepest = null;
            CmpVertex root = null;
            int maxDepth = 0;
            for (CmpVertex v : transitionMatrix.keySet()) {
                System.out.println(v);
                System.out.println("\t" + v.getDepth());
                if (v.getDepth() > maxDepth) {
                    deepest = v;
                    maxDepth = v.getDepth();
                } else if (v.getDepth() == 0) {
                    root = v;
                }
            }
            System.out.println("Deepest (" + maxDepth + ") == " + deepest);
            // Get the path to this node
            Collection<Label> path = getPathTo(deepest, root, transitionMatrix, new ArrayList<CmpVertex>());
            System.out.println("Path: " + path);
            // Get the alphabet
            Collection<ErlangLabel> alpha = new ArrayList<ErlangLabel>(moduleAlphabet);
            // Remove the elements that are examined for this node
            for (Label s : transitionMatrix.get(deepest).keySet()) {
                System.out.println("\tTried: " + s);
                alpha.remove(s);
            }
            System.out.println("Untried: " + alpha);
            if (alpha.size() > 0) {
                // Try all the others...
                for (ErlangLabel s : alpha) {
                    ArrayList<Label> trypath = new ArrayList<Label>(path);
                    trypath.add(s);
                    System.out.println("Trying " + trypath);
                    // Run this trace in Erlang and add the result to the traces file
                    Iterator<Label> it = trypath.iterator();
                    //System.out.println("Question for " + erlangModule + ":" + erlangWrapperModule + " is:");
                    String erlList = "[";
                    while (it.hasNext()) {
                        if (!erlList.equals("[")) {
                            erlList += ",";
                        }
                        erlList += it.next();
                    }
                    erlList += "]";
                    String erlArgs = "tracer2:first_failure(" + ErlangQSMOracle.erlangWrapperModule + "," + ErlangQSMOracle.erlangModule + "," + erlList + ",\"" + ErlangQSMOracle.tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(ErlangQSMOracle.erlangModules) + ")";
                    System.out.println("Evaluating " + erlArgs + " in folder " + ErlangQSMOracle.ErlangFolder);

                }
                System.out.println("##############################################################################");
                // FIXME stupid file sync issue...
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    ;
                }

                ErlangQSMOracle.loadCoverageMaps();
                ErlangQSMOracle.ErlangTraces = new PrefixTraceTree(ErlangQSMOracle.ErlangFolder + "/" + ErlangQSMOracle.tracesFile);
                // Strip wildcard traces from the file...
                wildCardStrip(ErlangFolder + File.separator + tracesFile);
                // For some reason this breaks if I re-use it...
                // I'm sure kirill will have a nice way to cary on from where we left off...
                tool = new QSMTool();
                tool.loadConfig(ErlangFolder + File.separator + tracesFile);
                QSMTool.setSimpleConfiguration(tool.learnerInitConfiguration.config, true, 0);
                innerLearner = new ErlangOracleLearner(viz, tool.learnerInitConfiguration,null);
                innerLearner.addObserver(viz);
                graph = innerLearner.learnMachine(tool.sPlus, tool.sMinus);
            } else {
                complete = true;
            }
        }

        // new PickNegativesVisualiser(new
        // SootCallGraphOracle()).construct(sPlus, sMinus,null, active);
        //config.setMinCertaintyThreshold(1);
        //config.setQuestionPathUnionLimit(1);

        return graph;
    }

    protected static Collection<Label> getPathTo(CmpVertex tgt, CmpVertex root, Map<CmpVertex, Map<Label, CmpVertex>> transitionMatrix, Collection<CmpVertex> seenStates) {
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
                    ArrayList<CmpVertex> newSeen = new ArrayList<CmpVertex>(seenStates);
                    newSeen.add(dest);
                    Collection<Label> subpath = getPathTo(tgt, dest, transitionMatrix, newSeen);
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
        //System.out.println("Stripping wildcards from " + filename);
        ArrayList<String> lines = new ArrayList<String>();
        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(filename));
            String line;
            while ((line = input.readLine()) != null) {
                if (line.indexOf("'*'") < 0) {
                    lines.add(line);
                    //System.out.println("Keeping " + line);
                } else {
                    //System.out.println("Stripping " + line);
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

    public static void createInitTraces() throws IOException {
        String erlArgs;
        erlArgs = "tracer2:gen_random_traces(" + erlangWrapperModule + "," + erlangModule + "," + initArgs + "," + erlangAlphabet + ",\"" + tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(erlangModules) + ")";
        //erlArgs = "tracer2:gen_exhaust_traces(" + erlangWrapperModule + "," + erlangModule + "," + initArgs + "," + erlangAlphabet + ",\"" + tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(erlangModules) + ")";

        System.out.println("Evaluating " + erlArgs + " in folder " + ErlangFolder);
        //./erlinittraces.sh testmod1 testfun [1,4,8,16,32,37,41,42] test2.out [testmod1,testmod2] in folder ErlangOracle
        //ErlangOracleLearner.runErlang(erlArgs);
    }

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
        //System.out.println("Loading coverage maps from " + filename + "...");
        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(filename));
            String line;
            while ((line = input.readLine()) != null) {
                // This assumes a format of [Trace] => [Coverage map]
                String[] toks = line.split("=>");

                Pair<Trace, Trace> index = new Pair<Trace, Trace>(new Trace(), Trace.fromString(toks[0].trim()));
                String map = toks[1].trim();
                map = map.substring(1, map.length() - 1);

                CodeCoverageMap mapObject = new CodeCoverageMap();
                if (map.length() > 0) {
                    // Create the parsed coverage map object
                    String[] maplets = (map.trim()).split("\\},\\{");
                    // Trim the {} off the first and last items...
                    maplets[0] = maplets[0].substring(1);
                    maplets[maplets.length - 1] = maplets[maplets.length - 1].substring(0, maplets[maplets.length - 1].length() - 1);
                    for (String m : maplets) {
                        // Maplets have the form {line, count} but should be missing the {} from the way we split the string.
                        String[] parts = m.split(",");
                        mapObject.add(parts[0], Integer.parseInt(parts[1]));
                    }

                }
                coverageMaps.put(index, mapObject);
                //System.out.println("Loading coverage map for \"" + index.secondElem + "\" = " + coverageMaps.get(index) + " (" + (coverageMaps.get(index) == null) + ")");
            }
            //System.out.println("Coverage maps:\n" + coverageMaps.toString());
        } catch (FileNotFoundException e) {
            System.out.println("Couldn't open coverage map file " + filename);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (input != null) {
                try {
                    input.close();
                } catch (IOException e) { /* ignore this */ }
            }
        }
        coverageMapLock = false;
    }
}