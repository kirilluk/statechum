package statechum.analysis.Erlang;

/**
 *
 * @author ramsay
 */
public class OTPGenServerBehaviour extends OTPBehaviour {

    public OTPGenServerBehaviour() {
        super();
        name = "gen_server";
        patterns.put("handle_cast", "cast");
        patterns.put("handle_call", "call");
        patterns.put("handle_info", "info");

    }
}
