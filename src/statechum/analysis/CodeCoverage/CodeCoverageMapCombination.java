/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.CodeCoverage;

import java.util.ArrayList;

/**
 *
 * @author ramsay
 */
public class CodeCoverageMapCombination {

    public ArrayList<CodeCoverageCombinedMaplet> map;

    public CodeCoverageMapCombination() {
        map = new ArrayList<CodeCoverageCombinedMaplet>();
    }

    @Override
    public String toString() {
        String result = "";
        for (CodeCoverageCombinedMaplet m : map) {
            result += m.toString() + ",";
        }
        return result;
    }
}
