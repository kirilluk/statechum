/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package statechum.analysis.Erlang.Signatures;

/**
 *
 * @author ramsay
 */
public class PortSignature extends Signature {

    public String instantiate() {
        //throw new RuntimeException("I don't know how to instantiate a port() signture...");
        return "PORT";
    }

}
