/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import statechum.Label;
import statechum.analysis.Erlang.Signatures.FuncSignature;

/**
 *
 * @author ramsay
 */
public class ErlangLabel extends OtpErlangTuple implements Label {

    /**
     * ID for serialization.
     */
    private static final long serialVersionUID = 5192814547774363649L;
    public final int arity;
    public final FuncSignature function;
    public final OtpErlangObject input, expectedOutput;

    public ErlangLabel(FuncSignature operator, OtpErlangObject inputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(operator.getQualifiedName()),
                    inputArgs
                });
        arity = 2;
        function = operator;
        input = inputArgs;
        expectedOutput = null;
    }

    @Override
    public String toString() {
        String result = function.toString() + "(" + input + ")";
        if (expectedOutput != null) {
            result = result + " == " + expectedOutput;
        }
        return result;
    }


	@Override
	public String toAlphaNum() {
		return function.toString()+"_"+input+"_" +
			(expectedOutput != null?" == " + expectedOutput:"");
	}

	public ErlangLabel(FuncSignature operator, OtpErlangObject inputArgs, OtpErlangObject expectedOutputArgs) {
        super(new OtpErlangObject[]{
                    new OtpErlangAtom(operator.getQualifiedName()),
                    inputArgs,
                    expectedOutputArgs
                });
        arity = 3;
        function = operator;
        input = inputArgs;
        expectedOutput = expectedOutputArgs;
    }

    @Override
    public int compareTo(Label other) {
        if (!(other instanceof ErlangLabel)) {
            throw new IllegalArgumentException("Comparing an ErlangLabel to something thats not an ErlangLabel");
        }

        ErlangLabel otherLabel = (ErlangLabel) other;

        int outcome = arity - otherLabel.arity;
        if (outcome != 0) {
            return outcome;
        }
        outcome = function.toString().compareTo(otherLabel.toString());
        if (outcome != 0) {
            return outcome;
        }
        outcome = input.toString().compareTo(otherLabel.input.toString());
        if (outcome != 0) {
            return outcome;
        }

        String ourExpectedOutput = expectedOutput == null ? "" : expectedOutput.toString();
        String otherExpectedOutput = otherLabel.expectedOutput == null ? "" : otherLabel.expectedOutput.toString();
        return ourExpectedOutput.compareTo(otherExpectedOutput);
    }


    /* No idea what this means, hence cannot fix
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
     */
    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + arity;
        result = prime * result
                + ((expectedOutput == null) ? 0 : expectedOutput.hashCode());
        result = prime * result
                + ((function == null) ? 0 : function.hashCode());
        result = prime * result + ((input == null) ? 0 : input.hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (!(obj instanceof ErlangLabel)) {
            return false;
        }
        ErlangLabel other = (ErlangLabel) obj;
        if (arity != other.arity) {
            return false;
        }
        if (expectedOutput == null) {
            if (other.expectedOutput != null) {
                return false;
            }
        } else if (!expectedOutput.equals(other.expectedOutput)) {
            return false;
        }
        if (function == null) {
            if (other.function != null) {
                return false;
            }
        } else if (!function.equals(other.function)) {
            return false;
        }
        if (input == null) {
            if (other.input != null) {
                return false;
            }
        } else if (!input.equals(other.input)) {
            return false;
        }
        return true;
    }
    
    /** Given a string containing the whole of the expression to parse, parses the text and returns the
     * corresponding Erlang label.
     *  
     * @param str label to parse
     * @return the outcome.
     */
    public static ErlangLabel parseLabel(String str)
    {
    	throw new UnsupportedOperationException("cannot parse yet");
    }
    
}
