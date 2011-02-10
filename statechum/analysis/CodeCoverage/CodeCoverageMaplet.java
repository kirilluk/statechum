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

    public String line;
    public int count;

    public CodeCoverageMaplet(String l, int c) {
        line = l;
        count = c;
    }

    @Override
    public String toString() {
        return "{" + line + " x " + count + "}";
    }
}
