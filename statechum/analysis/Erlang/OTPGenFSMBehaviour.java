/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

/**
 *
 * @author ramsay
 */
public class OTPGenFSMBehaviour extends OTPBehaviour {

    public OTPGenFSMBehaviour() {
        super();
        name = "gen_fsm";
        patterns.put("handle_event", "event");
        patterns.put("handle_sync_event", "sync");
        patterns.put("handle_info", "info");

    }
}
