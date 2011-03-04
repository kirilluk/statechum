/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import statechum.analysis.Erlang.Signatures.Signature;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;
import statechum.analysis.Erlang.Signatures.FailedToParseException;
import statechum.analysis.Erlang.Signatures.UnknownSignature;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.apps.ErlangApplicationLoader;

/**
 *
 * @author ramsay
 */
public class ErlangModule {

    public String name;
    public OTPBehaviour behaviour;
    public Map<String, FuncSignature> sigs;

    public ErlangModule() {
        name = "";
        sigs = new TreeMap<String, FuncSignature>();
    }

    public ErlangModule(String filename, File folder) throws IOException {
        this(new File(folder, filename));
    }

    protected String getFirstSpec(String buf) {
        int specstart = buf.indexOf("-spec");
        if (specstart < 0) {
            return null;
        }
        return buf.substring(specstart, buf.indexOf('\n', specstart));

    }

    public ErlangModule(File f) throws IOException {
        sigs = new TreeMap<String, FuncSignature>();

        // Compile and typecheck the module...
        ErlangApplicationLoader.dumpProcessOutput(Runtime.getRuntime().exec("erlc +debug_info " + f.getName(), null, f.getParentFile()));
        ErlangApplicationLoader.dumpProcessOutput(Runtime.getRuntime().exec("dialyzer --build_plt " + f.getName().replace(".erl", ".beam"), null, f.getParentFile()));
        // Receive the type info....
        Process p = Runtime.getRuntime().exec("typer " + f.getName(), null, f.getParentFile());
        ExperimentRunner.dumpStreams(p, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(@SuppressWarnings("unused") StringBuffer b) {
                System.err.print(b.toString());
            }

            @Override
            public void StdOut(@SuppressWarnings("unused") StringBuffer b) {
                String buf = b.toString();
                String spec = getFirstSpec(buf);
                while (spec != null) {
                    FuncSignature sig;
                    try {
                    sig = Signature.parseSignatureSpec(spec);
                    } catch (FailedToParseException e) {
                        sig = null;
                    }
                    sigs.put(sig.funcName, sig);
                    buf = buf.substring(spec.length());
                    spec = getFirstSpec(buf);
                }
            }
        });
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            ;
        }

        BufferedReader input = new BufferedReader(new FileReader(f));
        name = f.getName().substring(0, f.getName().lastIndexOf('.'));
        String line = "";
        while ((line != null) && ((line.length() <= 11) || (!(line.substring(0, 11)).equals("-behaviour(")))) {
            //System.out.println("Skipping " + line);
            line = input.readLine();
        }
        behaviour = new OTPUnknownBehaviour();
        if (line != null) {
            String bstring = line.substring(11, line.length() - 2);
            if (bstring.equals("gen_server")) {
                behaviour = new OTPGenServerBehaviour();
            } else if (bstring.equals("gen_event")) {
                behaviour = new OTPGenEventBehaviour();
            } else if (bstring.equals("gen_fsm")) {
                behaviour = new OTPGenFSMBehaviour();
            }
        }
        input.close();
        /*
        behaviour.loadInitArgs(f);
        behaviour.loadAlphabet(f);
        behaviour.loadDependencies(f);
         * *
         */
        behaviour.setModule(this);
        behaviour.loadInitArgs();
        behaviour.loadAlphabet();
        behaviour.loadDependencies(f);
        System.out.println("Module " + name + " init vals:");
        if(sigs.get("init") == null) {
            System.out.println("\tinit function not found.");
        } else {
        for (String s : sigs.get("init").instantiateAllArgs()) {
            System.out.println("\t" + s);
        }
        }
    }

    public String getName() {
        if (name == null) {
            return "";
        } else {
            return name;
        }
    }

    private String padRight(String s, int n) {
        String result = s;
        System.out.println(s + " (" + s.length() + " vs " + n + ")");
        while (result.length() < n) {
            result += " ";
        }
        return result;
    }

    @Override
    public String toString() {
        return getName() + " [" + behaviour.toString() + "] (" + behaviour.dependencies.size() + " dependecies)";
    }

    public Collection<String> getInitVals() {
        ArrayList<String> res = new ArrayList<String>();
        FuncSignature initSig = sigs.get("init");
        if (initSig != null) {
            for (String a : initSig.instantiateAllArgs()) {
                res.add("{init, " + a + "}");
            }
        }
        return res;
    }
}
