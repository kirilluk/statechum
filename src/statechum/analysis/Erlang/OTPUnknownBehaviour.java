/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import statechum.Configuration;
import statechum.analysis.Erlang.Signatures.FuncSignature;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

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
    
	/** 
	 * Used to take an existing function and generate an i/o pair for inclusion in an alphabet.
	 * 
	 * @param callName how the function should be called in traces. Important for OTP functions but should be the same as function name for ordinary exported functions. 
	 * @param function function to be associated with this i/o pair.
	 * @param config determines whether outputs are to be ignored.
	 */
	@Override
	void addFunctionToAlphabet(String callName, FuncSignature function, Configuration config)
	{
		List<List<OtpErlangObject>> args = function.instantiateAllArgs();
		List<OtpErlangObject> output = function.instantiateAllResults();

		for (List<OtpErlangObject> funcArgs : args) {
			OtpErlangList argsAsList = new OtpErlangList(funcArgs.toArray(new OtpErlangObject[]{}));
			
			if (config.getUseErlangOutputs()) {
				for (OtpErlangObject result : output) {
					alphabet.add(new ErlangLabel(parent.sigs.get(callName), callName, argsAsList, result));
				}
			} else {
				alphabet.add(new ErlangLabel(parent.sigs.get(callName),callName, argsAsList));
			}
		}
	}
    
	/**
	 * In an ordinary function called via <em>apply</em> or so, arguments are
	 * supplied in the form of a list. For Otp functions this is not the case -
	 * functions take a single argument. In order to moderate between the two, a
	 * conversion function is introduced which takes an Otp argument and turns
	 * it into pure Erlang one.
	 * 
	 * For an Unknown behaviour this should assert that an arg is a list and
	 * convert it into Java list, for Otp behaviours, we make singleton lists
	 * because all function take single arguments.
	 */
	@Override
	public List<OtpErlangObject> functionArgumentsToListOfArgs(OtpErlangObject arg) 
	{
		return Arrays.asList( ((OtpErlangList)arg).elements() );
	}
}
