/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package statechum.analysis.CodeCoverage;

/**
 *
 * @author ramsay
 */
public class CodeCoverageCombinedMaplet {
    public String line;
    public int count1;
    public int count2;

    public CodeCoverageCombinedMaplet(String l, int c1, int c2) {
        line = l;
        count1 = c1;
        count2 = c2;
    }

    @Override
    public String toString() {
        return "{" + line + " x " + count1 + "," + count2 + "}";
    }
}
