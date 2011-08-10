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
public class OTPGenFSMBehaviour extends OTPBehaviour {

	public OTPGenFSMBehaviour(ErlangModule mod) {
		this(mod, Configuration.getDefaultConfiguration());
	}

    public OTPGenFSMBehaviour(ErlangModule mod, Configuration config) {
        super(mod);
        name = "gen_fsm";
        /*
        patterns.put(parent.getName()+":handle_event/3", new Pair<String,Boolean>("event", Boolean.FALSE));
        patterns.put(parent.getName()+":handle_sync_event/2", new Pair<String,Boolean>("sync", Boolean.FALSE));
        patterns.put(parent.getName()+":handle_info/2", new Pair<String,Boolean>("info", Boolean.FALSE));
        patterns.put(parent.getName()+":init/1", 		new Pair<String,Boolean>("init", Boolean.FALSE));
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
