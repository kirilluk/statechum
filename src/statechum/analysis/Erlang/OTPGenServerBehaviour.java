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
        patterns.put("handle_cast", new Pair<String,Boolean>("cast", Boolean.FALSE));
        patterns.put("handle_call", new Pair<String,Boolean>("call", Boolean.TRUE));
        patterns.put("handle_info", new Pair<String,Boolean>("info", Boolean.FALSE));

    }
}
