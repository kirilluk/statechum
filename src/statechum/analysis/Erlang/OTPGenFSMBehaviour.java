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

    public OTPGenFSMBehaviour(ErlangModule mod) {
        super(mod);
        name = "gen_fsm";
        /*
        patterns.put(parent.getName()+":handle_event/3", new Pair<String,Boolean>("event", Boolean.FALSE));
        patterns.put(parent.getName()+":handle_sync_event/2", new Pair<String,Boolean>("sync", Boolean.FALSE));
        patterns.put(parent.getName()+":handle_info/2", new Pair<String,Boolean>("info", Boolean.FALSE));
        patterns.put(parent.getName()+":init/1", 		new Pair<String,Boolean>("init", Boolean.FALSE));
        */
        generateAlphabet();
    }
}
