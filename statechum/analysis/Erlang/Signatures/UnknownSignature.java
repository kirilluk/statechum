/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package statechum.analysis.Erlang.Signatures;

/**
 *
 * @author ramsay
 */
public class UnknownSignature extends Signature {

    public String value;

    public UnknownSignature(String v) {
        value = v;
    }

    public String instantiate() {
        return "UNKNOWN: " + value;
    }

}
