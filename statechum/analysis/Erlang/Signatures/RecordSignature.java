/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

/**
 *
 * @author ramsay
 */
public class RecordSignature extends Signature {

    public String name;
    public Map<String, Signature> attributes;

    public RecordSignature(String n) {
        name = n;
        attributes = new TreeMap<String, Signature>();
    }

    public String instantiate() {
        // FIXME Temp
        return "RECORD";
    }

    @Override
    public Collection<String> instantiateAllAlts() {
        ArrayList<String> result = new ArrayList<String>();
        // FIXME Temp

        result.add("RECORD");
        return result;
    }

}
