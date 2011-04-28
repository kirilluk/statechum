package statechum.analysis.Erlang;

import statechum.Pair;

/**
 *
 * @author ramsay
 */
public class OTPGenServerBehaviour extends OTPBehaviour {

    public OTPGenServerBehaviour(ErlangModule mod) {
        super(mod);
        name = "gen_server";
        patterns.put(mod.getName()+":handle_cast/2", new Pair<String,Boolean>("cast", Boolean.FALSE));
        patterns.put(mod.getName()+":handle_call/3", new Pair<String,Boolean>("call", Boolean.TRUE));
        patterns.put(mod.getName()+":handle_info/2", new Pair<String,Boolean>("info", Boolean.FALSE));
        patterns.put(mod.getName()+":init/1", 		new Pair<String,Boolean>("init", Boolean.FALSE));
        
        buildInvPatterns();
    }
}
