/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

/**
 *
 * @author ramsay
 */
public class OTPGenEventBehaviour extends OTPBehaviour {

    public OTPGenEventBehaviour() {
        super();
        name = "gen_event";
        patterns.put("handle_event", "event");
        patterns.put("handle_call", "call");
        patterns.put("handle_info", "info");
    }
}
