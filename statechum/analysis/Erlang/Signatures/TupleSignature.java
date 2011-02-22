/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author ramsay
 */
public class TupleSignature extends Signature {

    public Collection<Signature> elems;

    public TupleSignature() {
        super();
        elems = new ArrayList<Signature>();
    }

    public String instantiate() {
        String elemString = "";
        for (Signature e : elems) {
            if (!elemString.equals("")) {
                elemString += ",";
            }
            elemString += e.instantiate();
        }
        return "{" + elemString + "}";
    }

    @Override
    public Collection<String> instantiateAllAlts() {
        ArrayList<String> result = new ArrayList<String>();

        if (elems.size() > 0) {
            Signature head = elems.iterator().next();
            Collection<String> headVals = head.instantiateAllAlts();
            TupleSignature tail = new TupleSignature();
            tail.elems = new ArrayList<Signature>(elems);
            tail.elems.remove(head);
            Collection<String> tailVals = tail.instantiateAllAlts();
            for (String v : headVals) {
                if (tailVals.size() > 0) {
                    for (String t : tailVals) {
                        if (!t.equals("{}")) {
                        // Cut the {} off the sub tuple
                            result.add("{" + v + "," + t.substring(1, t.length() - 1) + "}");
                        } else {
                            result.add("{" + v + "}");
                        }
                    }
                } else {
                    result.add("{" + v + "}");
                }
            }
        } else {
            result.add("{}");
        }


        return result;

    }
}
