/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.CodeCoverage;

/**
 *
 * @author ramsay
 */
public class CodeCoverageMaplet {

    public int line;
    public int count;

    public CodeCoverageMaplet(int l, int c) {
        line = l;
        count = c;
    }

    @Override
    public String toString() {
        return "{" + line + " x " + count + "}";
    }
}
