/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.util.Collection;
import java.util.LinkedList;

/**
 *
 * @author ramsay
 */
public class ErlangApp {

    public String startModule;
    public String startModuleArgs;
    public String name;
    public Collection<ErlangModule> modules;
    public Collection<String> registered;

    public ErlangApp() {
        startModule = null;
        startModuleArgs = "[]";
        name = "";
        modules = new LinkedList<ErlangModule>();
        registered = new LinkedList<String>();
    }
}
