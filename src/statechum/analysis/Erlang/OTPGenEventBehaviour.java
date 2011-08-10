/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

import statechum.Configuration;
import statechum.analysis.Erlang.Signatures.FuncSignature;

/**
 * 
 * @author ramsay
 */
public class OTPGenEventBehaviour extends OTPBehaviour {

	public OTPGenEventBehaviour(ErlangModule mod) {
		this(mod, Configuration.getDefaultConfiguration());
	}

	public OTPGenEventBehaviour(ErlangModule mod, Configuration config) {
		super(mod);
		name = "gen_event";
		/*
		 * patterns.put("handle_event", new Pair<String,Boolean>("event",
		 * Boolean.FALSE)); patterns.put("handle_call", new
		 * Pair<String,Boolean>("call", Boolean.FALSE));
		 * patterns.put("handle_info", new Pair<String,Boolean>("info",
		 * Boolean.FALSE)); patterns.put("init", new
		 * Pair<String,Boolean>("init", Boolean.FALSE));
		 */
		generateAlphabet(config);
	}

	@SuppressWarnings("unused")
	@Override
	void addFunctionToAlphabet(String callName, FuncSignature function, Configuration config)
	{
		throw new UnsupportedOperationException("unimplemented");
	}

	@SuppressWarnings("unused")
	@Override
	public
	List<OtpErlangObject> functionArgumentsToListOfArgs(OtpErlangObject arg)
	{
		throw new UnsupportedOperationException("unimplemented");
	}
}
