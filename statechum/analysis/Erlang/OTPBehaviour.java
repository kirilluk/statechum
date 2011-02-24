/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import statechum.Pair;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import statechum.analysis.Erlang.Signatures.FuncSignature;

/**
 *
 * @author ramsay
 */
public abstract class OTPBehaviour {

    public String name;
    protected ErlangModule parent;
    protected Map<String, Pair<String, Boolean>> patterns;
    protected Collection<String> alphabet;
    protected Collection<String> dependencies;
    // FIXME these are currently undetermined...
    public Collection<String> initArgs;
    protected static final String[] stdmodsarray = {"erlang",
        "gen_server",
        "gen_fsm",
        "lists",
        "supervisor",
        "filename",
        "io",
        "io_lib",
        "code",
        "erl_ddll",
        "application",
        "math"};
    protected static final ArrayList<String> stdModsList = new ArrayList<String>(Arrays.asList(stdmodsarray));

    public OTPBehaviour() {
        name = "";
        alphabet = new ArrayList<String>();
        patterns = new TreeMap<String, Pair<String, Boolean>>();
        dependencies = new ArrayList<String>();
    }

    @Override
    public String toString() {
        if (name == null) {
            return "";
        } else {
            return name;
        }
    }

    public Collection<String> getDependencies() {
        return dependencies;
    }

    public void loadDependencies(File f) throws IOException {
        BufferedReader input = new BufferedReader(new FileReader(f));
        String line = "";
        while ((line = input.readLine()) != null) {
            if (!line.trim().startsWith("%%")) {
                // Hide any literal strings
                line = line.replaceAll("\"[^\"]*\"", "");
                while (line.indexOf(":") >= 0) {
                    String mod = line.substring(0, line.indexOf(":") + 1);
                    line = line.substring(line.indexOf(":") + 1);
                    String inverse = mod.replaceAll("[a-zA-Z_0-9<]*:", "");
                    mod = mod.replace(inverse, "");
                    mod = mod.substring(0, mod.length() - 1);
                    if (mod.length() < 2) {
                        line = line.substring(line.indexOf(":") + 1);
                    } else if (mod.indexOf('<') >= 0) {
                        // This is a binary thingie, not a module...
                        line = line.substring(line.indexOf(">>") + 2);
                    } else if (mod.substring(0, 1).equals(mod.substring(0, 1).toUpperCase())) {
                        // This is a variable module reference....
                        // This may break all our dep tracking...
                        line = line.substring(line.indexOf(":") + 1);
                    } else {
                        if (!stdModsList.contains(mod) && !dependencies.contains(mod)) {
                            dependencies.add(mod);
                        }
                    }
                }
            }
        }
    }

    public void setModule(ErlangModule mod) {
        parent = mod;
    }

    public void loadInitArgs() {
        initArgs = new ArrayList<String>();
        FuncSignature initSig = parent.sigs.get("init");
        if (initSig != null) {
            for (String a : initSig.instantiateAllArgs()) {
                initArgs.add("{init, " + a + "}");
            }
        }
    }

    public void loadAlphabet() {
        for (String p : patterns.keySet()) {
            FuncSignature sig = parent.sigs.get(p);
            if (sig != null) {
                for (String a : sig.instantiateAllArgs()) {
                    // I THINK we always want the first arg from these...
                                        Pair<String, Boolean> pat = patterns.get(p);
                    String op = "{" + pat.firstElem + ", " + a.split(",")[0];
                    if(pat.secondElem.booleanValue()) {
                        op += ", '*'";
                    }
                    op +=  "}";
                    alphabet.add(op);
                }
            }
        }
    }

    public Collection<String> getAlphabet() {
        return alphabet;
    }

    public String getAlphabetString() {
        String result = "[";
        boolean first = true;
        Iterator<String> it = alphabet.iterator();
        while (it.hasNext()) {
            if (!first) {
                result += ", ";
            } else {
                first = false;
            }
            result += it.next();
        }
        return result + "]";
    }
}
