/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.Collection;

/** This represents a set of alternative signatures.
 *
 * This is usually the parse of a BNF signature. So [ 58 | 59 , ... ]
 * is a ListSignature with the first elem an AltSignature with two LiteralSignature elements.
 *
 * @author ramsay
 */
public class AltSignature extends Signature {

    public Collection<Signature> elems;

    public AltSignature() {
        elems = new ArrayList<Signature>();
    }

    // Pick the first one...
    public String instantiate() {
        if (elems.size() <= 0) {
            throw new RuntimeException("Instantiating an un-initialised AltSignature...");
        } else {
            return elems.iterator().next().instantiate();
        }
    }

    @Override
    public Collection<String> instantiateAllAlts() {
        ArrayList<String> result = new ArrayList<String>();
        for(Signature s : elems) {
            result.addAll(s.instantiateAllAlts());
        }
        return result;
    }
}
