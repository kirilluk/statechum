/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 *
 * @author ramsay
 */
public class ErlangAppReader {

    private enum mode {

        MODULES, REGISTERED, NORMAL
    };

    public static ErlangApp readFolder(File folder) throws IOException {
	        ErlangApp result = new ErlangApp();
	        result.name = folder.getName();
	        for(File f: folder.listFiles()) {
	        	if (ErlangRunner.getErlName(f.getName()) != null)
	                result.modules.add(new ErlangModule(f));
	        }
	        return result;
    }

    public static ErlangApp readAppFile(String filename, File folder) throws IOException {
        ErlangApp result = new ErlangApp();
        result.name = ErlangRunner.getErlName(filename);

        BufferedReader input = new BufferedReader(new FileReader(new File(folder, filename)));
        String line = "";
        mode currentmode = mode.NORMAL;
        while ((line = input.readLine()) != null) {
            line = line.trim();
            if (currentmode == mode.NORMAL) {
                if (line.startsWith("{modules")) {
                    currentmode = mode.MODULES;
                } else if (line.startsWith("{registered")) {
                    currentmode = mode.REGISTERED;
                } else if (line.startsWith("{mod")) {
                    line = line.substring(line.indexOf("{", 4)+1);
                    String[] items = line.split(",");
                    result.startModule = items[0];
                    result.startModuleArgs = items[1].substring(0, items[1].indexOf("}"));
                }

                line = line.substring(line.indexOf("[") + 1);
            }
            if (currentmode == mode.MODULES) {
                String[] mods = line.split(",");
                for (String m : mods) {
                    if (m.indexOf("]") >= 0) {
                        m = m.substring(0, m.indexOf("]"));
                        currentmode = mode.NORMAL;
                    }
                    if(m.indexOf("'") < 0) {
                        try {
                            File f = new File(folder, m + ".erl");
                            result.modules.add(new ErlangModule(f));
                        } catch (FileNotFoundException e) {
                            throw new RuntimeException("File " + m + ".erl not found...");
                        }
                    }
                }
            }
            if (currentmode == mode.REGISTERED) {
                String[] mods = line.split(",");
                for (String m : mods) {
                    if (m.indexOf("]") >= 0) {
                        m = m.substring(0, m.indexOf("]"));
                        currentmode = mode.NORMAL;
                    }
                    result.registered.add(m);
                }
            }

        }

        return result;
    }
}
