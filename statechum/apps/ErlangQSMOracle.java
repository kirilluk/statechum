/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.apps;

import java.io.*;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;
import statechum.analysis.CodeCoverage.CodeCoverageMap;

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
    public static Collection<String> erlangModules;
    public static String erlangFunction;
    public static String erlangAlphabet;
    public static String tracesFile;
    public static String covermapFile;
    public static String ErlangFolder = "ErlangOracle";
    // This map stores coverage maps in the form (Prefix, Suffix) -> Coverage
    // i.e. the coverage map calculated from the end of trace Prefix to the end of state Suffix
    // The Map is indexed by the string representation of the prefix and suffix separated by a '-', in Erlang form
    // e.g. "[]-[a,b,c]" or "[a,b]-[a,b,c]"
    public static Map<String, CodeCoverageMap> coverageMaps;

    public static void main(String[] args) {
        // Generate some basic traces to get QSM started
        erlangModule = args[1];
        erlangFunction = args[2];
        tracesFile = args[0];
        covermapFile = tracesFile + ".covermap";
        erlangAlphabet = args[3];

        erlangModules = new LinkedList<String>();
        erlangModules.add(erlangModule);
        for(int i = 4; i < args.length; i++) {
            erlangModules.add(args[i]);
        }

        // Clear the files...
        (new File(ErlangFolder, tracesFile)).delete();
        (new File(ErlangFolder, covermapFile)).delete();

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
            String erlArgs = "tracer2:gen_random_traces(" + erlangModule + "," + erlangFunction + "," + erlangAlphabet + ",\"" + tracesFile + "\"," + ErlangOracleVisualiser.toErlangList(erlangModules)+")";
            //String erlCmd = "erl -eval 'tracer:gen_random_traces(" + erlangModule + "," + erlangFunction + "," + erlangAlphabet + ",\"" + tracesFile + "\"),halt().'\n";
            //System.out.println("Creating init traces...");
            System.out.println("Evaluating " + erlArgs + " in folder " + ErlangFolder);
            //./erlinittraces.sh testmod1 testfun [1,4,8,16,32,37,41,42] test2.out [testmod1,testmod2] in folder ErlangOracle
            ErlangOracleLearner.runErlang(erlArgs);
/*
            System.out.println("Traces file:");
            BufferedReader input = new BufferedReader(new FileReader(ErlangQSMOracle.ErlangFolder + "/" + tracesFile));
            String line = null;
            while ((line = input.readLine()) != null) {
                System.out.println(line);
            }
            input.close();
*/
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void loadCoverageMaps() {
        coverageMaps = new TreeMap<String, CodeCoverageMap>();
        loadCoverageMaps(ErlangFolder + "/" + covermapFile);
    }

    public static void loadCoverageMaps(String filename) {
        //System.out.println("Loading coverage maps from " + filename + "...");
        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(filename));
            String line;
            while ((line = input.readLine()) != null) {
                // This assumes a format of [prefix]-[suffix] => [Coverage map]
                String[] toks = line.split("=>");

                String index = toks[0].trim();
                //System.out.println("Loading coverage map for " + index);
                String map = toks[1].trim();
                map = map.substring(1, map.length() - 1);

                // Create the parsed coverage map object
                CodeCoverageMap mapObject = new CodeCoverageMap();
                String[] maplets = (map.trim()).split("\\},\\{");
                // Trim the {} off the first and last items...
                maplets[0] = maplets[0].substring(1);
                maplets[maplets.length - 1] = maplets[maplets.length - 1].substring(0, maplets[maplets.length - 1].length() - 1);
                for (String m : maplets) {
                    // Maplets have the form {line, count} but should be missing the {} from the way we split the string.
                    String[] parts = m.split(",");
                    mapObject.add(parts[0], Integer.parseInt(parts[1]));
                }

                coverageMaps.put(index, mapObject);
            }
            //System.out.println("Coverage maps:\n" + coverageMaps.toString());
        } catch (FileNotFoundException e) {
            System.out.println("Couldn't open coverage map file " + filename);
        } catch (IOException e) {
            e.printStackTrace();
        }
        finally
        {
            if (input != null)	try { input.close(); } catch (IOException e) { /* ignore this */ }
        }
    }
}
