/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.apps;

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

    public ErlangOracleRunner(String sf, String mn, String fn, String al, String om) {
            sourceFolder = sf;
            moduleName = mn;
            functionName = fn;
            Alphabet = al;
            otherModules = om;
    }

    public void run() {
        ErlangQSMOracle.ErlangFolder = sourceFolder;
        ErlangQSMOracle.main(new String[]{"test2.out", moduleName, functionName, Alphabet, otherModules});
    }
}
