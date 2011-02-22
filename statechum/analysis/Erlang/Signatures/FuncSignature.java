/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.Collection;

/** A signature for a function.
 *
 * Note: this is NOT a subclass of signature. Specifically has instantiateAllResults() and instantiateAllArgs() instead of instantiateAllAlts()
 *
 * @author ramsay
 */
public class FuncSignature {

    public String funcName = "";
    public Collection<ArrayList<Signature>> args;
    public Signature result;

    public FuncSignature() {
        args = new ArrayList<ArrayList<Signature>>();
    }

    public FuncSignature(String n) {
        this();
        funcName = n;
    }

    public String instantiate() {
        String argStrings = "";
        // Just pick the first pattern for now...
        // This needs to be cleverererer
        if (!args.isEmpty()) {
            for (Signature s : args.iterator().next()) {
                if (!argStrings.equals("")) {
                    argStrings += ",";
                }
                argStrings += s.instantiate();
            }
        }
        return funcName + "(" + argStrings + ")";
    }

    public Collection<String> instantiateAllResults() {
        return result.instantiateAllAlts();
    }

    private Collection<String> instantiateListOfArgs(Collection<Signature> args) {
        ArrayList<String> res = new ArrayList<String>();
        if (args.size() > 0) {
            Signature head = args.iterator().next();
            Collection<String> headVals = head.instantiateAllAlts();
            Collection<Signature> tail = new ArrayList<Signature>(args);
            tail.remove(head);
            Collection<String> tailVals = instantiateListOfArgs(tail);
            for (String h : headVals) {
                if (tailVals.size() > 0) {
                    for (String t : tailVals) {
                        if (t.length() > 0) {
                            res.add(h + "," + t);
                        } else {
                            res.add(h);
                        }
                    }
                } else {
                    res.add(h);
                }
            }


        }

        return res;
    }

    public Collection<String> instantiateAllArgs() {
        ArrayList<String> res = new ArrayList<String>();

        for (ArrayList<Signature> a : args) {
            res.addAll(instantiateListOfArgs(a));
        }

        return res;
    }
}
