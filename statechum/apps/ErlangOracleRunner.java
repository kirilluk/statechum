/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.apps;

import java.util.ArrayList;
import java.util.Collection;
import statechum.analysis.Erlang.ErlangModule;

/**
 *
 * @author ramsay
 */
public class ErlangOracleRunner implements Runnable {

    protected String sourceFolder;
    protected String moduleName;
    protected String functionName;
    protected String Alphabet;
    protected String otherModules;
    protected String mode = "basic";
    protected String initArgs = "";
    protected ErlangModule m;
    protected Collection<String> alphaSet = new ArrayList<String>();

    public ErlangOracleRunner(String sf, String mn, String fn, String al, String om) {
        sourceFolder = sf;
        moduleName = mn;
        functionName = fn;
        Alphabet = al;
        otherModules = om;
    }

    public ErlangOracleRunner(String sf, ErlangModule m, String om) {
        sourceFolder = sf;
        otherModules = om;
        mode = "otp";
        moduleName = m.name;
        functionName = m.behaviour.name + "_wrapper";
        Alphabet = m.behaviour.getAlphabetString();
        alphaSet = m.behaviour.getAlphabet();
        initArgs = "[";
        for(String a: m.behaviour.initArgs) {
            if(!initArgs.equals("[")) {
                initArgs += ",";
            }
            initArgs += a;
        }
        initArgs += "]";
    }

    public void run() {
        ErlangQSMOracle.ErlangFolder = sourceFolder;
        ErlangQSMOracle.mode = mode;
        ErlangQSMOracle.initArgs = initArgs;
        ErlangQSMOracle.moduleAlphabet = alphaSet;
        ErlangQSMOracle.main(new String[]{"test2.out", moduleName, functionName, Alphabet, otherModules});
    }
}
