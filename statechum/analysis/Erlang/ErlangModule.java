/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 * @author ramsay
 */
public class ErlangModule {

    public String name;
    public OTPBehaviour behaviour;

    public ErlangModule() {
    }

    public ErlangModule(String filename, File folder) throws IOException {
        this(new File(folder, filename));
    }

    public ErlangModule(File f) throws IOException {
        BufferedReader input = new BufferedReader(new FileReader(f));
        name = f.getName().substring(0, f.getName().lastIndexOf('.'));
        String line = "";
        while ((line != null) && ((line.length() <= 11) || (!(line.substring(0, 11)).equals("-behaviour(")))) {
            //System.out.println("Skipping " + line);
            line = input.readLine();
        }
        behaviour = new OTPUnknownBehaviour();
        if (line != null) {
            String bstring = line.substring(11, line.length() - 2);
            if (bstring.equals("gen_server")) {
                behaviour = new OTPGenServerBehaviour();
            } else if (bstring.equals("gen_event")) {
                behaviour = new OTPGenEventBehaviour();
            } else if (bstring.equals("gen_fsm")) {
                behaviour = new OTPGenFSMBehaviour();
            }
        }
        input.close();
        behaviour.loadInitArgs(f);
        behaviour.loadAlphabet(f);
        behaviour.loadDependencies(f);
    }

    public String getName() {
        if (name == null) {
            return "";
        } else {
            return name;
        }
    }

    private String padRight(String s, int n) {
        String result = s;
        System.out.println(s + " (" + s.length() + " vs " + n + ")");
        while (result.length() < n) {
            result += " ";
        }
        return result;
    }

    @Override
    public String toString() {
        return getName() + " [" + behaviour.toString() + "] (" + behaviour.dependencies.size() + " dependecies)";
    }
}
