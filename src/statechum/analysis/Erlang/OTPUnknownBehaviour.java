/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.io.File;

/**
 *
 * @author ramsay
 */
public class OTPUnknownBehaviour extends OTPBehaviour {

    public OTPUnknownBehaviour(ErlangModule mod) {
        super(mod);

        name = "UNKNOWN";
    }

    public void loadAlphabet(@SuppressWarnings("unused") File f) {
        return;
    }
}
