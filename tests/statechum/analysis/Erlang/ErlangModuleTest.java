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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import statechum.analysis.Erlang.ErlangRunner.ERL;
import statechum.analysis.Erlang.Signatures.AltSignature;
import statechum.analysis.Erlang.Signatures.AnySignature;
import statechum.analysis.Erlang.Signatures.AtomSignature;
import statechum.analysis.Erlang.Signatures.BinarySignature;
import statechum.analysis.Erlang.Signatures.BooleanSignature;
import statechum.analysis.Erlang.Signatures.ByteSignature;
import statechum.analysis.Erlang.Signatures.CharSignature;
import statechum.analysis.Erlang.Signatures.FailedToParseException;
import statechum.analysis.Erlang.Signatures.FuncSignature;
import statechum.analysis.Erlang.Signatures.IntSignature;
import statechum.analysis.Erlang.Signatures.ListSignature;
import statechum.analysis.Erlang.Signatures.LiteralSignature;
import statechum.analysis.Erlang.Signatures.PidSignature;
import statechum.analysis.Erlang.Signatures.PortSignature;
import statechum.analysis.Erlang.Signatures.RecordSignature;
import statechum.analysis.Erlang.Signatures.Signature;
import statechum.analysis.Erlang.Signatures.StringSignature;
import statechum.analysis.Erlang.Signatures.TupleSignature;
import statechum.analysis.Erlang.Signatures.UnknownSignature;
import statechum.analysis.learning.experiments.ExperimentRunner;
import statechum.analysis.learning.experiments.ExperimentRunner.HandleProcessIO;
import statechum.analysis.learning.rpnicore.LTL_to_ba;

public class ErlangModuleTest {
	
	@Test
	public void testRunParserFailure1()
	{
		statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() {
		public @Override void run() throws IOException {
			new ErlangModule(new File("ErlangExamples/WibbleMonster/wibble.erla"));
		}},IllegalArgumentException.class,"Invalid module");
	}

	@Test
	public void testRunParserFailure2()
	{
		final File file = new File("ErlangExamples/WibbleMonster/Wibble.erl");
		if (!file.canRead()) // running on a case-sensitive OS
			statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { 
				public @Override void run() throws IOException {
					new ErlangModule(file);
			}},IOException.class,"does not exist");
		else
			statechum.Helper.checkForCorrectException(new statechum.Helper.whatToRun() { 
				public @Override void run() throws IOException {
					new ErlangModule(file);
			}},RuntimeException.class,"Invalid file name");
	}
	
    /** Only used for testing against runTyperAsAProcessInsideErlang. 
     * @throws IOException */
    protected static String runTyperAsAProcess(File f) throws IOException
    {
    	ErlangRunner.compileErl(f, ErlangRunner.getRunner());
    	
        {// Now build environment variables to ensure that dialyzer will find a directory to put its plt file in.
	        Map<String,String> environment = System.getenv();
	        String [] envp = new String[environment.size()+1];int i=0;
	        for(Entry<String,String> entry:System.getenv().entrySet())
	        	envp[i++]=entry.getKey()+"="+entry.getValue();envp[i++]="HOME="+f.getParentFile().getAbsolutePath();
	
	        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"dialyzer","--build_plt","--output_plt",ErlangRunner.getName(f, ERL.PLT),ErlangRunner.getName(f, ERL.BEAM)}, envp, f.getParentFile());
	        ErlangRunner.dumpProcessOutputOnFailure("dialyzer",p);
        }
        // Receive the type info....
        Process p = Runtime.getRuntime().exec(new String[]{ErlangRunner.getErlangBin()+"typer","--plt",ErlangRunner.getName(f, ERL.PLT),ErlangRunner.getName(f, ERL.ERL)}, null, f.getParentFile());
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
        	throw new IllegalArgumentException("Failure running "+f.getName()+"\n"+err+(err.length()>0?"\n":"")+out);

       return out.toString();
    }
    
    /** Only used for testing against runTyperAsAProcess. 
     * @throws IOException */
    protected String runTyperAsAProcessInsideErlang(File f) throws IOException
    {
    	ErlangRunner erl = ErlangRunner.getRunner();
    	ErlangRunner.compileErl(f, erl);
    	OtpErlangTuple response = erl.call(
				new OtpErlangObject[]{new OtpErlangAtom("typer"), 
						new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.BEAM))}),
						new OtpErlangString(ErlangRunner.getName(f, ERL.PLT)),
						new OtpErlangList(new OtpErlangObject[]{new OtpErlangString(ErlangRunner.getName(f, ERL.ERL))}),
						new OtpErlangAtom("text")
					},
				"Could not run typer");
    	return ((OtpErlangString)response.elementAt(1)).stringValue();
    }
    
    @Test
    public void testConsistencyBetweenOriginalAndOurTyper1() throws IOException
    {
    	File file = new File("ErlangExamples/WibbleMonster/wibble.erl");
		new File(ErlangRunner.getName(file, ERL.PLT)).delete();
		String typerInRunner = runTyperAsAProcessInsideErlang(file).replace("\\\\", "\\");
		Assert.assertTrue(new File(ErlangRunner.getName(file, ERL.PLT)).delete());
		String typerAsProcess = runTyperAsAProcess(file).replace("\\\\", "\\");
		Assert.assertEquals(typerAsProcess,typerInRunner);
    }
    
    @Test
    public void testExtractFunctionTypes() throws IOException
    {
    	File file = new File("ErlangExamples/WibbleMonster/wibble.erl");
    	ErlangModule mod = new ErlangModule(file);
    	for(FuncSignature s:mod.sigs.values())
    	{
       		FuncSignature newSig = new FuncSignature(ErlangLabel.parseText(s.toErlangTerm()));
       		Assert.assertEquals(s, newSig);
       		Assert.assertEquals(s, new FuncSignature(ErlangLabel.parseText(newSig.toErlangTerm())));
    	}
    }
    
    
/*
	@Test
	public void testRunParser1() throws IOException
	{
		ErlangModule mod = new ErlangModule(new java.io.File("ErlangExamples/WibbleMonster/wibble.erl"));
		Assert.assertEquals("[{call, xyz, '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {call, [wibble], '*'}, {call, [wibble], '*'}, {call, [wibble], '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {call, xyz, '*'}, {cast, wibble}, {cast, stop}, {cast, xyz}, {cast, xyz3}, {info, wibble}, {info, xyz}]",
mod.behaviour.alphabet.toString());
	}
	*/
    protected static ListSignature parseList(StringBuffer specbuf, boolean definitelyNotEmpty,char terminal) {
        ListSignature lsig = null;
        List<Signature> elems = new LinkedList<Signature>();
        boolean empty = !definitelyNotEmpty;
        
        while (specbuf.charAt(0) != terminal) {
            elems.add(parseSignature(specbuf));
            bufTrimmer(specbuf);
            if (specbuf.charAt(0) == ',') {
                // More items...
                specbuf.delete(0, 1);
                bufTrimmer(specbuf);
            }
            if (specbuf.length() >= 3) {
                if (specbuf.substring(0, 3).equals("...")) {
                    // Undefined list continuation
                    specbuf.delete(0, 3);
                    bufTrimmer(specbuf);
                    empty = false;
                    // Almost certainly now ends...
                }
            }
        }
        specbuf.delete(0, 1);
        if (elems.isEmpty()) 
        	lsig = new ListSignature(new OtpErlangList(
        			empty?new OtpErlangObject[0]:new OtpErlangObject[]{ListSignature.NonEmptyAtom}),
        			new OtpErlangList());
        else 
        	lsig = new ListSignature(new OtpErlangList(),new OtpErlangList(elems.toArray(new OtpErlangObject[0]))); 
        return lsig;
    }

    protected static Signature parseSignature(StringBuffer specbuf) {
        //System.out.println(">>>> " + specbuf.toString());
        //System.out.flush();
        Signature sig;
        String spec = specbuf.toString();
        if (spec.startsWith("_")) {
            specbuf.delete(0, 1);
            sig = new AnySignature(new OtpErlangList());
        } else if (spec.matches("^[0-9].*")) {
            // Integer literal
            // Floats are not supported atm...
            String val = "";
            while ((specbuf.length() > 0) && (specbuf.substring(0, 1).matches("[0-9]"))) {
                val += "" + specbuf.substring(0, 1);
                specbuf.delete(0, 1);
            }
            // handle ranges...
            if (specbuf.toString().startsWith("..")) {
                int lowerValue = Integer.parseInt(val);
                specbuf.delete(0, 2);
                val = "";
                while ((specbuf.length() > 0) && (specbuf.substring(0, 1).matches("[0-9]"))) {
                    val += "" + specbuf.substring(0, 1);
                    specbuf.delete(0, 1);
                }
                int upperValue = Integer.parseInt(val);
                sig = new IntSignature(new OtpErlangList(),new OtpErlangList(new OtpErlangObject[]{
                		new OtpErlangInt(lowerValue),new OtpErlangInt(upperValue)
                }));

            } else {
                sig = new LiteralSignature(val);
            }
        } else if (spec.startsWith("any()")) {
            specbuf.delete(0, 5);
            sig = new AnySignature(new OtpErlangList());
        } else if (spec.startsWith("integer()")) {
            specbuf.delete(0, 9);
            sig = new IntSignature(new OtpErlangList());
        } else if (spec.startsWith("non_neg_integer()")) {
            specbuf.delete(0, 17);
            sig = new IntSignature(new OtpErlangList(new OtpErlangObject[]{IntSignature.NonNegativeAtom}));
        } else if (spec.startsWith("number()")) {
            specbuf.delete(0, 8);
            sig = new IntSignature(new OtpErlangList());
        } else if (spec.startsWith("pos_integer()")) {
            specbuf.delete(0, 13);
            sig = new IntSignature(new OtpErlangList(new OtpErlangObject[]{IntSignature.PositiveAtom}));
        } else if (spec.startsWith("binary()")) {
            specbuf.delete(0, 8);
            sig = new BinarySignature(new OtpErlangList());
        } else if (spec.startsWith("boolean()")) {
            specbuf.delete(0, 9);
            sig = new BooleanSignature(new OtpErlangList());
        } else if (spec.startsWith("atom()")) {
            specbuf.delete(0, 6);
            sig = new AtomSignature(new OtpErlangList());
        } else if (spec.startsWith("string()")) {
            specbuf.delete(0, 8);
            sig = new StringSignature(new OtpErlangList());
        } else if (spec.startsWith("tuple()")) {
            specbuf.delete(0, 7);
            sig = new TupleSignature(new OtpErlangList());
        } else if (spec.startsWith("char()")) {
            specbuf.delete(0, 6);
            sig = new CharSignature(new OtpErlangList());
        } else if (spec.startsWith("byte()")) {
            specbuf.delete(0, 6);
            sig = new ByteSignature(new OtpErlangList());
        } else if (spec.startsWith("pid()")) {
            specbuf.delete(0, 5);
            sig = new PidSignature();
        } else if (spec.startsWith("port()")) {
            specbuf.delete(0, 6);
            sig = new PortSignature();
        } else if (spec.startsWith("'")) {
            String lit = "";
            specbuf.delete(0, 1);
            while (specbuf.charAt(0) != '\'') {
                lit += specbuf.charAt(0);
                specbuf.delete(0, 1);
            }
            specbuf.delete(0, 1);
            sig = new LiteralSignature(lit);
        } else if (spec.startsWith("{")) {
            // Tuple...
            specbuf.delete(0, 1);
            bufTrimmer(specbuf);
            List<Signature> tupElems = new LinkedList<Signature>();
            while (specbuf.charAt(0) != '}') {
            	tupElems.add(parseSignature(specbuf));
                bufTrimmer(specbuf);
                if (specbuf.charAt(0) == ',') {
                    // More vals...
                    specbuf.delete(0, 1);
                    bufTrimmer(specbuf);
                }
            }
            // Swallow the closing }
            specbuf.delete(0, 1);
            sig = new TupleSignature(tupElems);
        } else if (spec.startsWith("maybe_improper_list(")) {
            specbuf.delete(0, 20);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, false, ')');
        } else if (spec.startsWith("nonempty_maybe_improper_list(")) {
            specbuf.delete(0, 29);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, true, ')');
        } else if (spec.startsWith("improper_list(")) {
            specbuf.delete(0, 14);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, false, ')');
        } else if (spec.startsWith("list(")) {
            specbuf.delete(0, 5);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, false, ')');
        } else if (spec.startsWith("[")) {
            // List spec
            specbuf.delete(0, 1);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, false, ']');
        } else if (spec.startsWith("#")) {
            // Record spec...
            // FIXME temp
            // Swallow the name if any
            String name = "";
            while (specbuf.charAt(0) != '{') {
                name += specbuf.substring(0, 1);
                specbuf.delete(0, 1);
            }
            RecordSignature rsig = new RecordSignature(new OtpErlangList(),new OtpErlangList());// this will throw since we provide no name for this record
            int depth = 1;
            while (depth > 0) {
                specbuf.delete(0, 1);
                bufTrimmer(specbuf);
                if (specbuf.charAt(0) == '{') {
                    depth += 1;
                } else if (specbuf.charAt(0) == '}') {
                    depth -= 1;
                }
            }
            specbuf.delete(0, 1);
            bufTrimmer(specbuf);
            sig = rsig;
        } else {
            // Something else...
            //System.out.println(specbuf.toString());
            // FIXME
            int end = specbuf.length();
            if (specbuf.indexOf(" ") > 0) {
                end = specbuf.indexOf(" ");
            }
            if ((specbuf.indexOf("]") < end) && (specbuf.indexOf("]") >= 0)) {
                end = specbuf.indexOf("]");
            }
            if ((specbuf.indexOf("|") < end) && (specbuf.indexOf("|") >= 0)) {
                end = specbuf.indexOf("|");
            }
            if ((specbuf.indexOf("}") < end) && (specbuf.indexOf("}") >= 0)) {
                end = specbuf.indexOf("}");
            }
            if ((specbuf.indexOf(")") < end) && (specbuf.indexOf(")") >= 0)) {
                end = specbuf.indexOf(")");
            }
            if ((specbuf.indexOf("(") < end) && (specbuf.indexOf("(") >= 0)) {
                end = specbuf.indexOf("(");
                int d = 1;
                while ((d > 0) && (end < specbuf.length())) {
                    end++;
                    if (specbuf.substring(end).startsWith(")")) {
                        d--;
                    } else if (specbuf.substring(end).startsWith("(")) {
                        d++;
                    }
                    //System.out.println(specbuf.substring(end) + " (depth: " + d + ")");
                }
            }


            String uk = specbuf.substring(0, end);
            specbuf.delete(0, end);
            if (specbuf.toString().startsWith(")")) {
                uk = uk + ")";
                specbuf.delete(0, 1);
            }
            sig = new UnknownSignature(uk);
        }
        bufTrimmer(specbuf);
        // Handle Alternates at this level...
        if (specbuf.length() > 0) {
            if (specbuf.charAt(0) == '|') {
                specbuf.delete(0, 1);
                bufTrimmer(specbuf);
                List<Signature> asigElems = new LinkedList<Signature>();
                asigElems.add(sig);
                Signature s = parseSignature(specbuf);
                if (s instanceof AltSignature) {
                	asigElems.addAll(((AltSignature) s).elems);
                } else {
                	asigElems.add(s);
                }
                sig = new AltSignature(asigElems);
            }
        }
        return sig;
    }

    private static void bufTrimmer(StringBuffer buf) {
        if ((buf.length() > 0)) {
            while ((buf.charAt(0) == ' ') || (buf.charAt(0) == '\t') || (buf.charAt(0) == '\n')) {
                buf.delete(0, 1);
                if (buf.length() <= 0) {
                    break;
                }
            }
        }
    }
    public static FuncSignature parseSignatureSpec(String specArg) throws FailedToParseException {
        String spec = specArg.trim();
        if (!spec.startsWith("-spec")) {
            throw new RuntimeException("Trying to parse a spec that isn't a spec...");
        }
        System.out.println(spec);
        String name = spec.substring(("-spec ").length(), spec.indexOf("("));
        int argsEnd = spec.substring(0, spec.lastIndexOf("->")).lastIndexOf(")");
        String args = spec.substring(("-spec ").length() + name.length() + 1, argsEnd).trim();
        String res = spec.substring(spec.lastIndexOf("->") + 2).trim();
        
        System.out.println("Function: " + name);
        List<List<Signature>> argList = new LinkedList<List<Signature>>();
        StringBuffer argbuf = new StringBuffer(args);
        List<Signature> argset = new LinkedList<Signature>();
        while (argbuf.length() > 0) {
            Signature a = parseSignature(argbuf);
            argset.add(a);
            bufTrimmer(argbuf);
            if (argbuf.length() > 0) {
                if (argbuf.charAt(0) == '|') {
                    // Another pattern...
                	argList.add(argset);
                    argset = new ArrayList<Signature>();
                    argbuf.delete(0, 1);
                    bufTrimmer(argbuf);
                } else if (argbuf.charAt(0) == ',') {
                    // Another arg...
                    argbuf.delete(0, 1);
                    bufTrimmer(argbuf);
                } else if (argbuf.charAt(0) == ')') {
                    // Finished...??
                    argbuf.delete(0, 1);
                    bufTrimmer(argbuf);
                } else {
                    // Er, what?
                    throw new FailedToParseException("Unparsable char '" + argbuf.charAt(0) + "' on the front of " + argbuf.toString());

                }
            }
        }
        argList.add(argset);

        List<Signature> resultValue = new LinkedList<Signature>();
        StringBuffer resbuf = new StringBuffer(res);
        while (resbuf.length() > 0) {
            Signature a = parseSignature(resbuf);
            bufTrimmer(resbuf);
            if (resbuf.length() > 0) 
            {
                if (resbuf.charAt(0) == '|') {
                    // More possible result types
                    resbuf.delete(0, 1);
                    bufTrimmer(resbuf);
                    resultValue.add(a);
                } else if (resbuf.charAt(0) == '.') {
                    // Finished...
                    resbuf.delete(0, 1);
                    bufTrimmer(resbuf);
                    resultValue.add(a);
                } else {
                    // Er, what?
                    throw new RuntimeException("Unparsable char '" + resbuf.charAt(0) + "'");
                }
            } else {
            	resultValue.add(a);
            }

        }
        assert !resultValue.isEmpty();
        FuncSignature result = new FuncSignature("UNKNOWN",name,argList,
        		resultValue.size()>1?new AltSignature(resultValue):resultValue.get(0));
        System.out.println("Args: " + result.instantiateAllArgs().size() + " possibilities");
        
        boolean firstline = true;
        for (List<OtpErlangObject> a : result.instantiateAllArgs()) {
	        if (!firstline) {
	        System.out.println("|");
	        } else {
	        firstline = false;
	        }
	        System.out.println(a);
        }
        System.out.println();
        System.out.println("Result: " + result.instantiateAllResults().size() + " possibilities");

        return result;
    }
    
    protected String getFirstSpec(String buf) {
        int specstart = buf.indexOf("-spec");
        if (specstart < 0) {
            return null;
        }
        return buf.substring(specstart, buf.indexOf('\n', specstart));

    }
    
    public Map<String,FuncSignature> parseErlang(String bufArg)
    {
    	String buf = bufArg;
    	Map<String,FuncSignature> sigs=  new TreeMap<String,FuncSignature>();
        String spec = getFirstSpec(buf);
        while (spec != null) {
            FuncSignature sig;
            try {
                sig = parseSignatureSpec(spec);
                //sig.argInstances.addAll(seekUsages(sig.funcName, f));
                sigs.put(sig.getName(), sig);
            } catch (FailedToParseException e) {
                sig = null;
            }
            buf = buf.substring(spec.length());
            spec = getFirstSpec(buf);
        }
        return sigs;
    }
}
