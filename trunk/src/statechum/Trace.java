/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author ramsay
 */
public class Trace implements Comparable<Trace> {

    List<Label> trace;
    public boolean negative;

    public Trace() {
        this(new LinkedList<Label>(),true);
    }

    public List<Label> getList() {
    	return trace;
    }
    
    @SuppressWarnings("unused")
	public static Trace fromString(String s)
    {
    	throw new UnsupportedOperationException("cannot build a collection of traces from a string yet");
    }
    
    @SuppressWarnings("unused")
	public static Trace fromListOfStrings(List<String> s)
    {
    	throw new UnsupportedOperationException("cannot build a collection of traces from a collection of strings yet");
    }
    
    public Trace(Collection<Label> c, boolean neg) {
        trace = new LinkedList<Label>(c);
        negative = neg;
    }
    
    @Override
    public Trace clone() {
        return new Trace(new LinkedList<Label>(trace),negative);
    }

    public void add(Label s) {
        trace.add(s);
    }

    public int size() {
        return trace.size();
    }

    /*
    public Trace replaceAll(String x, String y) {
        Trace result = new Trace();
        for (Label s : trace) {
            result.add(s.replaceAll(x, y));
        }
        return result;
    }

     * 
     */
    @Override
	public int compareTo(Trace o) {
    	// FIXME: should not be done with toString()
        return this.toString().compareTo(o.toString());
    }

    public Iterator<Label> iterator() {
        return trace.iterator();
    }

    public int indexOf(Label s) {
        return trace.indexOf(s);
    }

    public Label get(int index) {
        return trace.get(index);
    }

    @Override
    public String toString() {
        return trace.toString();
    }

    public String toTraceString() {
        String result = "";
        if (negative) {
            result = "-";
        } else {
            result = "+";
        }
        for (Label s : trace) {
            result += " " + s.toString();
        }
        return result;
    }

    protected static String wildcard = "'*'";

    public static boolean matchWithWildcard(Trace x, Trace y) {
        if (x.equals(y)) {
            return true;
        }
        Iterator<Label> it = x.iterator();
        Iterator<Label> tit = y.iterator();
        while (it.hasNext()) {
            if (!tit.hasNext()) {
                return false;
            } else {
                Label xs = it.next();
                Label ys = tit.next();
                if (!matchWithWildcard(xs.toString(), ys.toString())) {
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
        //System.out.println("\t" + x + " == " + wildcardplace);
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
        Iterator<Label> sit = this.iterator();
        Iterator<Label> eit = et.iterator();
        while (sit.hasNext()) {
            if (eit.hasNext()) {
                Label snext = sit.next();
                Label enext = eit.next();
                if (!snext.equals(enext)) {
                    // Lets try wildcard matching
                    // Prefixes will always be instantiated up until the last element...
                    //System.out.println("\t\t" + enext + " vs " + snext + " == " + oneWayWildcardMatch(enext, snext));
                    if (!oneWayWildcardMatch(enext.toString(), snext.toString())) {
                        // If this IS the last element then we are allowed a wildcard at the end
                        // This SHOULD only happen for negative traces where the last event fails so the output cant be instantiated
                        if (!sit.hasNext()) {
                            //System.out.println("\t\t\tFinally: " + snext + " vs " + enext + " == " + oneWayWildcardMatch(snext, enext));
                            return oneWayWildcardMatch(snext.toString(), enext.toString());
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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (negative ? 1231 : 1237);
		result = prime * result + ((trace == null) ? 0 : trace.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Trace other = (Trace) obj;
		if (negative != other.negative)
			return false;
		if (!trace.equals(other.trace))
			return false;
		return true;
	}
}
