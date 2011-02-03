/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package analysis.Erlang;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageMaplet {

    public int line;
    public int count;

    public ErlangCoverageMaplet(int l, int c) {
        line = l;
        count = c;
    }

    @Override
    public String toString() {
        return "{" + line + " x " + count + "}";
    }
}
