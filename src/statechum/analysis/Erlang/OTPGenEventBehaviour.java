/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import statechum.Pair;

/**
 *
 * @author ramsay
 */
public class OTPGenEventBehaviour extends OTPBehaviour {

    public OTPGenEventBehaviour() {
        super();
        name = "gen_event";
        patterns.put("handle_event", new Pair<String,Boolean>("event", Boolean.FALSE));
        patterns.put("handle_call", new Pair<String,Boolean>("call", Boolean.FALSE));
        patterns.put("handle_info", new Pair<String,Boolean>("info", Boolean.FALSE));
    }
}
