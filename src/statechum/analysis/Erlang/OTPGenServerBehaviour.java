package statechum.analysis.Erlang;

import java.util.List;

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
    		
           	/** No conversion for return type. */
        	@Override
    		public Signature extractVisibleReturnType(Signature fullReturnType)
        	{
        		return fullReturnType;
        	}
      	
        	/** No conversion for arguments. */
        	@Override
    		public List<List<Signature>> convertArguments(List<List<Signature>> args)
        	{
        		return args;
        	}
        });
        
    }
}
