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

    public static ErlangApp readFolder(File folder) {
        try 
        {
	        ErlangApp result = new ErlangApp();
	        result.name = folder.getName();
	        for(File f: folder.listFiles()) {
	        	if (ErlangModule.getErlName(f.getName()) != null)
	                result.modules.add(new ErlangModule(f));
	        }
	        return result;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static ErlangApp readAppFile(String filename, File folder) {
        ErlangApp result = new ErlangApp();
        result.name = filename.substring(0, filename.lastIndexOf("."));
        try {
            BufferedReader input = new BufferedReader(new FileReader(new File(folder, filename)));
            String line = "";
            mode currentmode = mode.NORMAL;
            while ((line = input.readLine()) != null) {
                line = line.trim();
                if (currentmode == mode.NORMAL) {
                    if ((line.length() > 8) && (line.substring(0, 8).equals("{modules"))) {
                        currentmode = mode.MODULES;
                    } else if ((line.length() > 11) && (line.substring(0, 11).equals("{registered"))) {
                        currentmode = mode.REGISTERED;
                    } else if ((line.length() > 4) && (line.substring(0, 4).equals("{mod"))) {
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
                                System.out.println("File " + m + ".erl not found...");
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
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(3);
        }

        return result;
    }
}
