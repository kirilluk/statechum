/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

/**
 *
 * @author ramsay
 */
public class IntSignature extends Signature {

    public boolean negative = true;
    public int lower;
    public int upper;

    public String instantiate() {
        return "" + lower;
    }
}
