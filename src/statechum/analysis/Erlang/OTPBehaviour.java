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

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import statechum.Pair;
import java.io.File;
import java.io.IOException;
import java.util.*;
import statechum.analysis.Erlang.Signatures.FuncSignature;

/**
 *
 * @author ramsay
 */
public abstract class OTPBehaviour {

    public String name;
    final protected ErlangModule parent;
    protected Map<String, Pair<String, Boolean>> patterns;
    protected List<OtpErlangTuple> alphabet;
    protected Collection<String> dependencies;
    public Collection<List<OtpErlangObject>> initArgs;
    protected static final String[] stdmodsarray = {"erlang",
        "gen_server",
        "gen_fsm",
        "lists",
        "supervisor",
        "filename",
        "io",
        "io_lib",
        "code",
        "erl_ddll",
        "application",
        "math"};
    protected static final ArrayList<String> stdModsList = new ArrayList<String>(Arrays.asList(stdmodsarray));

    public OTPBehaviour(ErlangModule mod) {
    	name = null;parent = mod;
    	alphabet = new LinkedList<OtpErlangTuple>();
        patterns = new TreeMap<String, Pair<String, Boolean>>();
        dependencies = new LinkedList<String>();
    }

    @Override
    public String toString() {
        return name;
    }
    
    public String getWrapperName()
    {
    	return name + "_wrapper";
    }
    
    public Collection<String> getDependencies() {
        return dependencies;
    }

    /** Extracts dependencies of the supplied module, assuming the module has been successfully compiled and .beam file exists.
     * 
     * @param file the file of the module
     * @throws IOException if this fails.
     */
    public void loadDependencies(File file)
    {
    	OtpErlangTuple response = ErlangRunner.getRunner().call(new OtpErlangObject[]{
    			new OtpErlangAtom("dependencies"),
    			new OtpErlangAtom(ErlangRunner.getName(file, ErlangRunner.ERL.BEAM))},
    			"Could not load dependencies of "+file.getName());

    	OtpErlangList listOfDepTuples = (OtpErlangList)response.elementAt(1);// the first element is 'ok'
    	for(OtpErlangObject tup:listOfDepTuples.elements())
    	{
    		String mod = ( (OtpErlangAtom) ((OtpErlangTuple)tup).elementAt(0)).atomValue();
            if (!stdModsList.contains(mod) && !dependencies.contains(mod)) {
               dependencies.add(mod);
            }
        }
    }

    public void loadInitArgs() {
        initArgs = new LinkedList<List<OtpErlangObject>>();
        FuncSignature initSig = parent.sigs.get(parent.getName()+":init/1");
        if (initSig != null) // stopgap solution
        	initArgs.addAll(initSig.instantiateAllArgs());
        /*
        if (initSig != null) {
            for (String a : initSig.instantiateAllArgs()) {
                // Skip values with variables since we cant instantiate them...
                if ((!(a.matches("^[_A-Z].*") || a.matches(".* [_A-Z].*")))&&(a.length() > 0)) {
                    System.out.println("Init: " + a);
                    initArgs.add("{init, " + a + "}");
                } else {
                    System.out.println("Excluding Init: " + a);
                }
            }
        }*/
    }

    public void loadAlphabet() {
    	for(FuncSignature sig:parent.sigs.values())
    		if (patterns.keySet().contains(sig.getName())) 
    		for(List<OtpErlangObject> funcArgs:sig.instantiateAllArgs())
    		{
    			if (funcArgs.isEmpty()) throw new RuntimeException("function "+sig+" should take at least one argument");
    			OtpErlangObject firstArg = funcArgs.get(0);
    			alphabet.add(new ErlangLabel(sig,firstArg));
    		}
    }

    public List<OtpErlangTuple> getAlphabet() {
        return alphabet;
    }

}
