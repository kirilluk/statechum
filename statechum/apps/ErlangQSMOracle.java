/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.apps;

import java.io.*;
import java.lang.String;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import statechum.analysis.Erlang.ErlangCoverageMap;

import statechum.analysis.learning.*;

/**
 *
 * @author ramsay
 */
/** Extends QSMTool to use an Erlang oracle to answer the questions...
 *
 * @author ramsay
 */
public class ErlangQSMOracle extends QSMTool {

    public static String erlangModule;
    public static String erlangFunction;
    public static String erlangAlphabet;
    public static String tracesFile;
    public static String ErlangFolder = "ErlangOracle";
    // This map stores coverage maps in the form (Prefix, Suffix) -> Coverage
    // i.e. the coverage map calculated from the end of trace Prefix to the end of state Suffix
    // The Map is indexed by the string representation of the prefix and suffix separated by a '-', in Erlang form
    // e.g. "[]-[a,b,c]" or "[a,b]-[c,d,e]"
    public static Map<String, ErlangCoverageMap> coverageMaps;

    public static void main(String[] args) {
        // Generate some basic traces to get QSM started
        erlangModule = args[1];
        erlangFunction = args[2];
        tracesFile = args[0];
        erlangAlphabet = args[3];

        // Clear the files...
        (new File(ErlangFolder, tracesFile)).delete();
        (new File(ErlangFolder, tracesFile + ".covermap")).delete();

        createInitTraces();
        loadCoverageMaps();

        ErlangQSMOracle tool = new ErlangQSMOracle();
        tool.loadConfig(ErlangFolder + "/" + tracesFile);
        tool.runExperiment();
    }

    @Override
    public void runExperiment() {
        setSimpleConfiguration(learnerInitConfiguration.config, active, k);
        if (learnerInitConfiguration.ifthenSequences != null && !learnerInitConfiguration.ifthenSequences.isEmpty()) {
            learnerInitConfiguration.config.setUseLTL(true);
        }
        if (learnerInitConfiguration.labelDetails != null) {
            sPlus.addAll(learnerInitConfiguration.labelDetails.getSPlus());
            sMinus.addAll(learnerInitConfiguration.labelDetails.getSMinus());
        }
        // This is the one line thats actually changed...
        ErlangOracleVisualiser pnv = new ErlangOracleVisualiser();
        pnv.construct(sPlus, sMinus, learnerInitConfiguration);

        pnv.startLearner(null);
        // new PickNegativesVisualiser(new
        // SootCallGraphOracle()).construct(sPlus, sMinus,null, active);
        //config.setMinCertaintyThreshold(1);
        //config.setQuestionPathUnionLimit(1);
    }

    public static void createInitTraces() {
        try {
            String erlCmd = "./erlinittraces.sh " + erlangModule + " " + erlangFunction + " " + erlangAlphabet + " " + tracesFile;
            //String erlCmd = "erl -eval 'tracer:gen_random_traces(" + erlangModule + "," + erlangFunction + "," + erlangAlphabet + ",\"" + tracesFile + "\"),halt().'\n";
            //System.out.println("Running " + erlCmd + " in folder " + ErlangFolder);
            Process p = Runtime.getRuntime().exec(erlCmd, null, new File(ErlangFolder));
            System.out.println("Creating init traces...");
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            //System.out.println("Process output:");
            String line;
            while ((line = input.readLine()) != null) {
                //System.out.println(line);
            }
            input.close();

            p.waitFor();

            System.out.println("Traces file:");
            input = new BufferedReader(new FileReader(ErlangQSMOracle.ErlangFolder + "/" + tracesFile));

            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            input.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void loadCoverageMaps() {
        coverageMaps = new TreeMap<String, ErlangCoverageMap>();
        System.out.println("Loading coverage maps...");
        try {
            BufferedReader input = new BufferedReader(new FileReader(ErlangFolder + "/" + tracesFile + ".covermap"));
            //System.out.println("Process output:");
            String line;
            while ((line = input.readLine()) != null) {
                // This assumes a format of [prefix]-[suffix] => [Coverage map]
                String[] toks = line.split("=>");

                String index = toks[0].trim();
                //System.out.println("Loading coverage map for " + index);
                String map = toks[1].trim();
                map = map.substring(1, map.length() - 1);
                /*
                String[] traces = toks[0].split("\\]-\\[");
                String prefix = (traces[0].trim()).substring(1);
                String suffix = traces[1].trim();
                suffix = suffix.substring(0, suffix.length() - 1);
          
                Collection<String>[] index = (Collection<String>[]) Array.newInstance(Collection.class, 2);
                index[0] = Arrays.asList(prefix.split(","));
                index[1] = Arrays.asList(suffix.split(","));

                 *
                 */

                // Create the parsed coverage map object
                ErlangCoverageMap mapObject = new ErlangCoverageMap();
                String[] maplets = (map.trim()).split("\\},\\{");
                // Trim the {} off the first and last items...
                maplets[0] = maplets[0].substring(1);
                maplets[maplets.length - 1] = maplets[maplets.length - 1].substring(0,maplets[maplets.length - 1].length() - 1);
                for(String m: maplets) {
                    // Maplets have the form {line, count} but should be missing the {} from the way we split the string.
                    String[] parts = m.split(",");
                    mapObject.add(Integer.parseInt(parts[0]),Integer.parseInt(parts[1]));
                }

                coverageMaps.put(index, mapObject);
            }
            input.close();
            //System.out.println("Coverage maps:\n" + coverageMaps.toString());
        } catch (FileNotFoundException e) {
            System.out.println("Couldn't open coverage map file " + tracesFile + ".covermap");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
