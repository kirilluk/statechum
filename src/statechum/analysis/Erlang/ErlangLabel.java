/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangTuple;
import statechum.Label;

/**
 *
 * @author ramsay
 */
public class ErlangLabel extends OtpErlangTuple implements Label {

    public final int arity;

    public ErlangLabel(String operator, String input) {
        super(new OtpErlangAtom[]{
                    new OtpErlangAtom(operator),
                    new OtpErlangAtom(input)
                });
        arity = 2;
    }

    public ErlangLabel(String operator, String input, String expectedOutput) {
        super(new OtpErlangAtom[]{
                    new OtpErlangAtom(operator),
                    new OtpErlangAtom(input),
                    new OtpErlangAtom(expectedOutput)
                });
        arity = 3;
    }

    public int compareTo(Label other) {
        if (!(other instanceof ErlangLabel)) {
            throw new RuntimeException("Comparing an ErlangLabel to something thats not an ErlangLabel");
        } else {
            return this.toString().compareTo(other.toString());
        }
    }

    public ErlangLabel replaceAll(String from, String to) {
        String[] resultContent = new String[arity];
        for (int i = 0; i < arity; i++) {
            resultContent[i] = this.elementAt(i).toString().replaceAll(from, to);
        }
        // Only arity 2 or arity 3 elements should exist...
        // This would be better handled by having two subclasses that represent the different types of Alphabet element. Maybe Ill do that tomorrow.
        if (arity == 2) {
            return new ErlangLabel(resultContent[0], resultContent[1]);
        } else {
            return new ErlangLabel(resultContent[0], resultContent[1], resultContent[2]);

        }
    }
}
