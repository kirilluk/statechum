/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

/**
 *
 * @author ramsay
 */
public class Trace implements Comparable<Trace> {

    LinkedList<String> trace;

    public Trace() {
        trace = new LinkedList<String>();
    }

    public Trace(Collection<String> c) {
        trace = new LinkedList<String>(c);
    }

    protected String consumeErlangElem(StringBuffer sb) {
        String elem = "";
        if (sb.charAt(0) == '{') {
            // Consume a tuple
            sb.deleteCharAt(0);
            elem += "{";
            elem += deepConsume(sb, '}');
            elem += "}";
            sb.deleteCharAt(0);
        } else if (sb.charAt(0) == '[') {
            // Consume a list
            sb.deleteCharAt(0);
            elem += "[";
            elem += deepConsume(sb, ']');
            elem += "]";
            sb.deleteCharAt(0);
        } else if (sb.charAt(0) == '\'') {
            // consume a literal
            sb.deleteCharAt(0);
            elem += "'";
            while (sb.charAt(0) != '\'') {
                elem += consumeErlangElem(sb);
            }
            elem += "'";
            sb.deleteCharAt(0);
        } else if (sb.charAt(0) == '"') {
            // consume a literal
            sb.deleteCharAt(0);
            elem += "\"";
            while (sb.charAt(0) != '"') {
                elem += consumeErlangElem(sb);
            }
            elem += "\"";
            sb.deleteCharAt(0);
        } else {
            // CONSUME! until a magic char...
            while ((sb.charAt(0) != ',')
                    && (sb.charAt(0) != '}')
                    && (sb.charAt(0) != ']')
                    && (sb.charAt(0) != '\'')
                    && (sb.charAt(0) != '"')) {
                //System.out.println("\tConsuming " + sb.substring(0, 1));
                elem += sb.substring(0, 1);
                sb.deleteCharAt(0);
            }
        }
        //System.out.println("Consumed " + elem);
        return elem;
    }

    private String deepConsume(StringBuffer sb, char end) {
        String elem = "";
        while (sb.charAt(0) != end) {
            elem += consumeErlangElem(sb);
            if (sb.charAt(0) == ',') {
                elem += ",";
                sb.deleteCharAt(0);
            }
        }
        //System.out.println("Deep consumed " + elem);
        return elem;
    }

    /** Parse a trace from a string.
     * This expects a String of the form "[elem, elem, elem, ...]"
     * This is aware of Erlang style sub-sontainers like [] and {} and quotations...
     * @param t The String to be parsed
     */
    public Trace(String t) {
        this();
        //System.out.println("Parsing " + t);
        StringBuffer sb = new StringBuffer(t);
        if (sb.charAt(0) != '[') {
            throw new RuntimeException("Cannot parse Trace from " + t);
        }
        sb.deleteCharAt(0);
        while (sb.charAt(0) != ']') {
            String elem = consumeErlangElem(sb);
            //System.out.println(elem);
            trace.add(elem);
            if (sb.charAt(0) == ',') {
                sb.deleteCharAt(0);
            }
        }
    }

    @Override
    public Trace clone() {
        return new Trace((LinkedList<String>) trace.clone());
    }

    public void add(String s) {
        trace.add(s);
    }

    public int size() {
        return trace.size();
    }

    public Trace replaceAll(String x, String y) {
        Trace result = new Trace();
        for (String s : trace) {
            result.add(s.replaceAll(x, y));
        }
        return result;
    }

    public int compareTo(Trace o) {
        if (o instanceof Trace) {
            if (this.equals((Trace) o)) {
                return 0;
            } else {
                return -1;
            }
        } else {
            return -1;
        }
    }

    public Iterator<String> iterator() {
        return trace.iterator();
    }

    public int indexOf(String s) {
        return trace.indexOf(s);
    }

    @Override
    public String toString() {
        return trace.toString();
    }

    public boolean equals(Trace tr) {
        Iterator<String> it = trace.iterator();
        Iterator<String> tit = trace.iterator();
        while (it.hasNext()) {
            if (!tit.hasNext()) {
                return false;
            } else {
                if (!(it.next().equals(tit.next()))) {
                    return false;
                }
            }
        }
        return !tit.hasNext();
    }
    protected static String wildcard = "'*'";

    public static boolean matchWithWildcard(Trace x, Trace y) {
        Iterator<String> it = x.iterator();
        Iterator<String> tit = y.iterator();
        while (it.hasNext()) {
            if (!tit.hasNext()) {
                return false;
            } else {
                String xs = it.next();
                String ys = tit.next();
                if (!matchWithWildcard(xs, ys)) {
                    return false;
                }
            }
        }
        return !tit.hasNext();
    }

    public static boolean matchWithWildcard(String x, String y) {
        //System.out.println("Wildcarding \"" + x + "\" and \"" + y + "\" --- " + (oneWayWildcardMatch(x, y) || oneWayWildcardMatch(y, x)));
        return oneWayWildcardMatch(x, y) || oneWayWildcardMatch(y, x);
    }

    public static boolean oneWayWildcardMatch(String x, String y) {
        //System.out.println("\t\t \"" + x + "\" and \"" + y + "\"");
        // We now have to allow for wildcards in output specs
        // we are using the string "'*'" as the wildcard...
        int wildcardplace = x.indexOf(wildcard);
        if (wildcardplace >= 0) {
            // We need to replace the wildcard with what we DID find...
            String pre = x.substring(0, wildcardplace);
            String post = x.substring(wildcardplace + wildcard.length());
            // Potentially there are more wildcards...
            if ((y.length() < pre.length()) || (post.length() > y.length())) {
                return false;
            }
            String preY = y.substring(0, pre.length());
            String postY = y.substring(y.length() - post.length());
            // Evaluate the left part FIRST!!
            if (!preY.equals(pre)) {
                return false;
            }
            return oneWayWildcardMatch(post, postY);
        } else {
            return x.equals(y);
        }
    }

    /**
     * Determines whether this is a prefix of et.
     */
    public boolean isPrefix(Trace et) {
        Iterator<String> sit = this.iterator();
        Iterator<String> eit = et.iterator();
        while (sit.hasNext()) {
            if (eit.hasNext()) {
                String snext = sit.next();
                String enext = eit.next();
                if (!snext.equals(enext)) {
                    // Lets try wildcard matching
                    // Prefixes will always be instantiated up until the last element...
                    //System.out.println("\t\t" + enext + " vs " + snext + " == " + oneWayWildcardMatch(enext, snext));
                    if (!oneWayWildcardMatch(enext, snext)) {
                        // If this IS the last element then we are allowed a wildcard at the end
                        // This SHOULD only happen for negative traces where the last event fails so the output cant be instantiated
                        if(!sit.hasNext()) {
                            //System.out.println("\t\t\tFinally: " + snext + " vs " + enext + " == " + oneWayWildcardMatch(snext, enext));
                            return oneWayWildcardMatch(snext, enext);
                        } else {
                            return false;
                        }
                    }
                }
            } else {
                return false;
            }
        }
        return true;
    }
}
