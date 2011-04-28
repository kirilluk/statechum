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
public class OTPGenFSMBehaviour extends OTPBehaviour {

    public OTPGenFSMBehaviour(ErlangModule mod) {
        super(mod);
        name = "gen_fsm";
        patterns.put("handle_event", new Pair<String,Boolean>("event", Boolean.FALSE));
        patterns.put("handle_sync_event", new Pair<String,Boolean>("sync", Boolean.FALSE));
        patterns.put("handle_info", new Pair<String,Boolean>("info", Boolean.FALSE));
        patterns.put("init", 		new Pair<String,Boolean>("init", Boolean.FALSE));

    }
}
