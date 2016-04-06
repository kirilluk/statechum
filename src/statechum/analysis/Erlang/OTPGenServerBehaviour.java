package statechum.analysis.Erlang;

import java.util.Collections;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;

import statechum.Configuration;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.Erlang.Signatures.Signature;

/**
 *
 * @author ramsay
 */
public class OTPGenServerBehaviour extends OTPBehaviour {

    public OTPGenServerBehaviour(ErlangModule mod) {
        super(mod);
        name = "gen_server";
        patterns.put(mod.getName()+":handle_cast/2", new OTPCall()
        {
    		@Override
    		public String getOtpName() {
    			return "cast";
    		}
        });
        
        patterns.put(mod.getName()+":handle_call/3", new OTPCall()
        {
        	
        });
        
        patterns.put(mod.getName()+":handle_info/2", new OTPCall()
        {
    		@Override
    		public String getOtpName() {
    			return "info";
    		}
    		
        	/** No conversion for return type. */
        	@Override
    		public Signature extractVisibleReturnType(Signature fullReturnType)
        	{
        		return fullReturnType;
        	}
        });
        patterns.put(mod.getName()+":init/1",new OTPCall()
        {
    		@Override
    		public String getOtpName() {
    			return "init";
    		}
    		
           	/** Only the first element of the return value is to be returned. */
        	@Override
    		public Signature extractVisibleReturnType(Signature fullReturnType)
        	{
        		return Signature.extractElement(fullReturnType, 0);
        	}
      	
        	/** No conversion for arguments. */
        	@Override
    		public List<List<Signature>> convertArguments(List<List<Signature>> args)
        	{
        		return args;
        	}
        });
        
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
			if (funcArgs.size() != 1)
				throw new IllegalArgumentException("OTP function "+function+" has non-singleton arguments");
			OtpErlangObject argsAsObject = funcArgs.get(0);
			
			if (config.getUseErlangOutputs()) {
				for (OtpErlangObject result : output) {
					alphabet.add(new ErlangLabel(parent.sigs.get(callName), callName, argsAsObject, result));
				}
			} else {
				alphabet.add(new ErlangLabel(parent.sigs.get(callName),callName, argsAsObject));
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
		return Collections.singletonList(arg);
	}
}
