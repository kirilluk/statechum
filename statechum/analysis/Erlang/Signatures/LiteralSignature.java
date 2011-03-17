/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

/**
 *
 * @author ramsay
 */
public class LiteralSignature extends Signature {

    public String value;

    public LiteralSignature(String v) {
        value = v;
    }

    public String instantiate() {
        return "'" + value + "'";
    }
}
