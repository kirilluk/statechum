/* Copyright (c) 2011 The University of Sheffield.
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
 * 
 */
package statechum.analysis.Erlang;

import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 *
 * @author ramsay
 */
public class ErlangModule {

	
	public File sourceFolder;
    public String name;
    public OTPBehaviour behaviour;
    public Map<String, FuncSignature> sigs;

    public ErlangModule() {
        name = null;
        sigs = new TreeMap<String, FuncSignature>();
    }

    public ErlangModule(String filename, File folder) throws IOException {
        this(new File(folder, filename));
    }
    
    public static final String behaviourToken = "-behaviour(";

    public ErlangModule(final File f) throws IOException 
    {
    	name = ErlangRunner.getName(f,ERL.MOD);
    	sourceFolder = f.getParentFile();
        System.out.println("----------------  " + name + "  --------------------------");

        // Compile and typecheck the module...
        ErlangRunner.compileErl(f,ErlangRunner.getRunner());
        sigs = new TreeMap<String, FuncSignature>();
        
		new File(ErlangRunner.getName(f, ERL.PLT)).delete();
		OtpErlangTuple response = null;
		 response = ErlangRunner.getRunner().call(
				new OtpErlangObject[]{new OtpErlangAtom("typer"), 
						new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.BEAM))}),
						new OtpErlangString(ErlangRunner.getName(f, ERL.PLT)),
						new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.ERL))}),
						new OtpErlangAtom("types")
					},
				"Could not run typer");
       OtpErlangList analysisResults = (OtpErlangList)response.elementAt(1);
        Assert.assertEquals(1,analysisResults.arity());
        OtpErlangTuple fileDetails = (OtpErlangTuple)analysisResults.elementAt(0);
        OtpErlangList typeInformation = (OtpErlangList) fileDetails.elementAt(3);
        for(int i=0;i<typeInformation.arity();++i)
        {
        	FuncSignature s = new FuncSignature(typeInformation.elementAt(i));
        	sigs.put(s.toString(), s);
        }

        BufferedReader input = new BufferedReader(new FileReader(f));
        String line = input.readLine();
        while (line != null && !line.startsWith(behaviourToken)) {
            //System.out.println("Skipping " + line);
            line = input.readLine();
        }
        behaviour = new OTPUnknownBehaviour(this);
        if (line != null) {
            String bstring = line.substring(behaviourToken.length());
            if (bstring.startsWith("gen_server")) {
                behaviour = new OTPGenServerBehaviour(this);
            } else if (bstring.startsWith("gen_event")) {
                behaviour = new OTPGenEventBehaviour(this);
            } else if (bstring.startsWith("gen_fsm")) {
                behaviour = new OTPGenFSMBehaviour(this);
            }
        }
        input.close();
        /*
        behaviour.loadInitArgs(f);
        behaviour.loadAlphabet(f);
        behaviour.loadDependencies(f);
         * *
         */
        behaviour.loadInitArgs();
        behaviour.loadAlphabet();
        behaviour.loadDependencies(f);
    }

    private static Collection<String> seekUsages(String funcName, File f) {
        Collection<String> result = new ArrayList<String>();

        // Open the Erlang source files...
        try {
            BufferedReader input = new BufferedReader(new FileReader(f));
            String line = null;
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

    public String getName() 
    {
    	assert name != null;
        return name;
    }

    @Override
    public String toString() {
        return getName() + " [" + behaviour.toString() + "] (" + behaviour.dependencies.size() + " dependecies)";
    }

}
