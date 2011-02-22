/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang.Signatures;

import java.util.ArrayList;
import java.util.Collection;

/** Describes the type signature of a function and provides some useful(?) methods for manipulating them.
 *
 * This is deliberately polymorphic with subclasses to support progressive inference of signatures.
 *
 * @author ramsay
 */
public abstract class Signature {

    protected static ListSignature parseList(StringBuffer specbuf, char terminal) {
        ListSignature lsig = new ListSignature();
        while (specbuf.charAt(0) != terminal) {
            lsig.elems.add(parseSignature(specbuf));
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
                    lsig.empty = false;
                    // Almost certainly now ends...
                }
            }
        }
        specbuf.delete(0, 1);
        return lsig;
    }

    protected static Signature parseSignature(StringBuffer specbuf) {
        Signature sig;
        String spec = specbuf.toString();
        if (spec.startsWith("_")) {
            specbuf.delete(0, 1);
            sig = new AnySignature();
        } else if (spec.matches("^[0-9].*")) {
            // Integer literal
            // Floats are not supported atm...
            String val = "";
            while (specbuf.substring(0, 1).matches("[0-9]")) {
                val += "" + specbuf.substring(0, 1);
                specbuf.delete(0, 1);
            }
            sig = new LiteralSignature(val);
        } else if (spec.startsWith("any()")) {
            specbuf.delete(0, 5);
            sig = new AnySignature();
        } else if (spec.startsWith("integer()")) {
            specbuf.delete(0, 9);
            sig = new IntSignature();
        } else if (spec.startsWith("non_neg_integer()")) {
            specbuf.delete(0, 17);
            sig = new IntSignature();
            ((IntSignature) sig).negative = false;
        } else if (spec.startsWith("number()")) {
            specbuf.delete(0, 8);
            sig = new IntSignature();
        } else if (spec.startsWith("binary()")) {
            specbuf.delete(0, 8);
            sig = new BinarySignature();
        } else if (spec.startsWith("boolean()")) {
            specbuf.delete(0, 9);
            sig = new BooleanSignature();
        } else if (spec.startsWith("atom()")) {
            specbuf.delete(0, 6);
            sig = new AtomSignature();
        } else if (spec.startsWith("string()")) {
            specbuf.delete(0, 8);
            sig = new StringSignature();
        } else if (spec.startsWith("tuple()")) {
            specbuf.delete(0, 7);
            sig = new TupleSignature();
        } else if (spec.startsWith("char()")) {
            specbuf.delete(0, 6);
            sig = new CharSignature();
        } else if (spec.startsWith("byte()")) {
            specbuf.delete(0, 6);
            sig = new ByteSignature();
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
            TupleSignature tsig = new TupleSignature();
            bufTrimmer(specbuf);
            while (specbuf.charAt(0) != '}') {
                tsig.elems.add(parseSignature(specbuf));
                bufTrimmer(specbuf);
                if (specbuf.charAt(0) == ',') {
                    // More vals...
                    specbuf.delete(0, 1);
                    bufTrimmer(specbuf);
                }
            }
            // Swallow the closing }
            specbuf.delete(0, 1);
            sig = tsig;
        } else if (spec.startsWith("maybe_improper_list(")) {
            specbuf.delete(0, 20);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, ')');
        } else if (spec.startsWith("nonempty_maybe_improper_list(")) {
            specbuf.delete(0, 29);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, ')');
            ((ListSignature) sig).empty = false;
        } else if (spec.startsWith("improper_list(")) {
            specbuf.delete(0, 14);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, ')');
        } else if (spec.startsWith("list(")) {
            specbuf.delete(0, 5);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, ')');
        } else if (spec.startsWith("[")) {
            // List spec
            specbuf.delete(0, 1);
            bufTrimmer(specbuf);
            sig = parseList(specbuf, ']');
        } else if (spec.startsWith("#")) {
            // Record spec...
            // FIXME temp
            // Swallow the name if any
            String name = "";
            while (specbuf.charAt(0) != '{') {
                name += specbuf.substring(0, 1);
                specbuf.delete(0, 1);
            }
            RecordSignature rsig = new RecordSignature(name);
            int depth = 1;
            while (depth > 0) {
                specbuf.delete(0, 1);
                bufTrimmer(specbuf);
                if (specbuf.charAt(0) == '{') {
                    depth += 1;
                    System.out.println("depth: " + depth);
                } else if (specbuf.charAt(0) == '}') {
                    depth -= 1;
                    System.out.println("depth: " + depth);
                }
            }
            specbuf.delete(0, 1);
            bufTrimmer(specbuf);
            sig = rsig;
        } else {
            // Something else...

            // FIXME
            String uk = specbuf.substring(0, 1);
            specbuf.delete(0, 1);
            sig = new UnknownSignature(uk);
        }
        bufTrimmer(specbuf);
        // Handle Alternates at this level...
        if (specbuf.length() > 0) {
            if (specbuf.charAt(0) == '|') {
                specbuf.delete(0, 1);
                bufTrimmer(specbuf);
                AltSignature asig = new AltSignature();
                asig.elems.add(sig);
                Signature s = parseSignature(specbuf);
                if (s instanceof AltSignature) {
                    asig.elems.addAll(((AltSignature) s).elems);
                } else {
                    asig.elems.add(s);
                }
                sig = asig;
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

    public static FuncSignature parseSignatureSpec(String spec) {
        spec = spec.trim();
        if (!spec.startsWith("-spec")) {
            throw new RuntimeException("Trying to parse a spec that isn't a spec...");
        }
        System.out.println(spec);
        String name = spec.substring(("-spec ").length(), spec.indexOf("("));
        int argsEnd = spec.lastIndexOf(")", spec.indexOf("->"));
        String args = spec.substring(("-spec ").length() + name.length() + 1, argsEnd).trim();
        String res = spec.substring(spec.indexOf("->") + 2).trim();
        FuncSignature result = new FuncSignature(name);
        System.out.println("Function: " + result.funcName);
        StringBuffer argbuf = new StringBuffer(args);
        ArrayList<Signature> argset = new ArrayList<Signature>();
        while (argbuf.length() > 0) {
            Signature a = parseSignature(argbuf);
            argset.add(a);
            bufTrimmer(argbuf);
            if (argbuf.length() > 0) {
                if (argbuf.charAt(0) == '|') {
                    // Another pattern...
                    result.args.add(argset);
                    argset = new ArrayList<Signature>();
                    argbuf.delete(0, 1);
                    bufTrimmer(argbuf);
                } else if (argbuf.charAt(0) == ',') {
                    // Another arg...
                    argbuf.delete(0, 1);
                    bufTrimmer(argbuf);
                } else {
                    // Er, what?
                    throw new RuntimeException("Unparsable char '" + argbuf.charAt(0) + "' on the front of " + argbuf.toString());
                }
            }
        }
        result.args.add(argset);
        System.out.println("Args: " + result.instantiateAllArgs().size() + " possibilities");
        /*
        boolean firstline = true;
        for (String a : result.instantiateAllArgs()) {
        if (!firstline) {
        System.out.println("|");
        } else {
        firstline = false;
        }
        System.out.println(a);
        }
        System.out.println("");

         *
         */
        StringBuffer resbuf = new StringBuffer(res);
        while (resbuf.length() > 0) {
            Signature a = parseSignature(resbuf);
            bufTrimmer(resbuf);
            if (resbuf.length() > 0) {
                if (resbuf.charAt(0) == '|') {
                    // More possible result types
                    resbuf.delete(0, 1);
                    bufTrimmer(resbuf);
                    if (result.result == null) {
                        result.result = new AltSignature();
                    }
                    ((AltSignature) result.result).elems.add(a);
                } else if (resbuf.charAt(0) == '.') {
                    // Finished...
                    resbuf.delete(0, 1);
                    bufTrimmer(resbuf);
                    if (result.result == null) {
                        result.result = a;
                    } else {
                        ((AltSignature) result.result).elems.add(a);
                    }
                } else {
                    // Er, what?
                    throw new RuntimeException("Unparsable char '" + resbuf.charAt(0) + "'");
                }
            } else {
                if (result.result == null) {
                    result.result = a;
                } else {
                    ((AltSignature) result.result).elems.add(a);
                }
            }

        }
        System.out.println("Result: " + result.instantiateAllResults().size() + " possibilities");
        /*
        firstline = true;
        for (String a : result.instantiateAllResults()) {
        if (!firstline) {
        System.out.println("|");
        } else {
        firstline = false;
        }
        System.out.println(a);
        }
        System.out.println("");
         *
         */

        return result;
    }

    /** This method should provide a valid (if pointless) instance of the type signature it represents.
     *
     * Currently it will always use 1 for integers and wibble for atoms, and corresponding useful values like [1] and [wibble] for lists.
     *
     */
    public abstract String instantiate();

    /** This method should provide a collection with one possible instantiation for each available alternate signature in the parse.
     *
     * Most signatures will just use instantiate() but should provide one instance for each of their possible children's instances.
     *
     * This should be overridden as necessary by subclasses.
     */
    public Collection<String> instantiateAllAlts() {
        ArrayList<String> result = new ArrayList<String>();
        result.add(this.instantiate());
        return result;
    }
}
