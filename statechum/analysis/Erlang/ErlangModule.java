/* Copyright (c) 2011 Ramsay Taylor and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */
package statechum.analysis.Erlang;

import statechum.analysis.Erlang.Signatures.Signature;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import statechum.analysis.Erlang.Signatures.FailedToParseException;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.LTL_to_ba;
import statechum.apps.ErlangApplicationLoader;

/**
 *
 * @author ramsay
 */
public class ErlangModule {

    public String name;
    public OTPBehaviour behaviour;
    public Map<String, FuncSignature> sigs;

    public ErlangModule() {
        name = "";
        sigs = new TreeMap<String, FuncSignature>();
    }

    public ErlangModule(String filename, File folder) throws IOException {
        this(new File(folder, filename));
    }

    protected String getFirstSpec(String buf) {
        int specstart = buf.indexOf("-spec");
        if (specstart < 0) {
            return null;
        }
        return buf.substring(specstart, buf.indexOf('\n', specstart));

    }
    
    public static final String behaviourToken = "-behaviour(";
    
    public ErlangModule(final File f) throws IOException {
    	name = ErlangRunner.getErlName(f.getName());if (name == null) throw new IllegalArgumentException("invalid Erlang file name "+f.getName());
        System.out.println("----------------  " + name + "  --------------------------");
        sigs = new TreeMap<String, FuncSignature>();

        // Compile and typecheck the module...
        ErlangRunner.compileErl(f);
        final String pltFileName = f.getParentFile().getAbsolutePath()+File.separator+name+".plt";
        // Now build environment variables to ensure that dialyzer will find a directory to put its plt file in.

        Map<String,String> environment = System.getenv();
        String [] envp = new String[environment.size()+1];int i=0;
        for(Entry<String,String> entry:System.getenv().entrySet())
        	envp[i++]=entry.getKey()+"="+entry.getValue();envp[i++]="HOME="+f.getParentFile().getAbsolutePath();

        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"dialyzer","--build_plt","--output_plt",pltFileName,name+".beam"}, envp, f.getParentFile());
        ErlangApplicationLoader.dumpProcessOutputOnFailure("dialyzer",p);

        // Receive the type info....
        p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"typer","--plt",pltFileName,f.getName()}, null, f.getParentFile());
    	final StringBuffer err=new StringBuffer(),out=new StringBuffer(); 
        ExperimentRunner.dumpStreams(p, LTL_to_ba.timeBetweenHearbeats, new HandleProcessIO() {

            @Override
            public void OnHeartBeat() {// no prodding is done for a short-running converter.
            }

            @Override
            public void StdErr(StringBuffer b) {
                err.append(b);
            }

            @Override
            public void StdOut(StringBuffer b) {
            	out.append(b);
            }
        });
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            ;
        }
        if (p.exitValue() != 0)
        	throw new IllegalArgumentException("Failure running "+name+"\n"+err+(err.length()>0?"\n":"")+out);

        String buf = out.toString();
        String spec = getFirstSpec(buf);
        while (spec != null) {
            FuncSignature sig;
            try {
                sig = Signature.parseSignatureSpec(spec);
                sig.argInstances.addAll(seekUsages(sig.funcName, f));
                sigs.put(sig.funcName, sig);
            } catch (FailedToParseException e) {
                sig = null;
            }
            buf = buf.substring(spec.length());
            spec = getFirstSpec(buf);
        }

        BufferedReader input = new BufferedReader(new FileReader(f));
        String line = input.readLine();
        while (line != null && !line.startsWith(behaviourToken)) {
            //System.out.println("Skipping " + line);
            line = input.readLine();
        }
        behaviour = new OTPUnknownBehaviour();
        if (line != null) {
            String bstring = line.substring(behaviourToken.length());
            if (bstring.startsWith("gen_server")) {
                behaviour = new OTPGenServerBehaviour();
            } else if (bstring.startsWith("gen_event")) {
                behaviour = new OTPGenEventBehaviour();
            } else if (bstring.startsWith("gen_fsm")) {
                behaviour = new OTPGenFSMBehaviour();
            }
        }
        input.close();
        /*
        behaviour.loadInitArgs(f);
        behaviour.loadAlphabet(f);
        behaviour.loadDependencies(f);
         * *
         */
        behaviour.setModule(this);
        behaviour.loadInitArgs();
        behaviour.loadAlphabet();
        behaviour.loadDependencies(f);
    }

    private static Collection<String> seekUsages(String funcName, File f) {
        Collection<String> result = new ArrayList<String>();

        // Open the Erlang source files...
        try {
            BufferedReader input = new BufferedReader(new FileReader(f));
            String line = "";
            while ((line = input.readLine()) != null) {
                // Look for calls to this func
                int ptr = line.indexOf(funcName + "(");
                while (ptr >= 0) {
                    System.out.println("Got call to " + funcName + " on line \"" + line + "\"");
                    int depth = 1;
                    ptr += (funcName + "(").length();
                    int start = ptr;
                    while ((depth > 0)&&(ptr <= line.length())) {
                        if((ptr == line.length())&&(depth > 0)) {
                            String newLine = input.readLine();
                            if(newLine != null) {
                                line += newLine;
                            }
                        }
                        // Allow for () in the argstring itself...
                        if (line.charAt(ptr) == '(') {
                            depth++;
                        } else if (line.charAt(ptr) == ')') {
                            depth--;
                        }
                        ptr++;
                    }
                    ptr--;
                    // Add to argument string to the result list
                    result.add(line.substring(start, ptr));
                    System.out.println("\t" + line.substring(start, ptr));
                    line = line.substring(ptr);
                    ptr = line.indexOf(funcName + "(");
                    // Loop for more occurences on this line
                }
            }
            input.close();
        } catch (IOException e) {
            ;
        }

        return result;
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
