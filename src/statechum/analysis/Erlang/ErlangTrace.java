/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package statechum.analysis.Erlang;

import java.util.Collection;
import java.util.LinkedList;
import statechum.Label;
import statechum.Trace;

/**
 *
 * @author ramsay
 */
public class ErlangTrace extends Trace {
    
    public static Trace parseTrace(Collection<String> c) {
        Collection<Label> result = new LinkedList<Label>();
        for(String s : c) {
            result.add(ErlangLabel.parseLabel(s));
        }
        return new Trace(result);
    }


}
