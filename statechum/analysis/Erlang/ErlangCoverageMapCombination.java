/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package analysis.Erlang;

import java.util.ArrayList;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageMapCombination {

    public ArrayList<ErlangCoverageCombinedMaplet> map;

    public ErlangCoverageMapCombination() {
        map = new ArrayList<ErlangCoverageCombinedMaplet>();
    }

    @Override
    public String toString() {
        String result = "";
        for (ErlangCoverageCombinedMaplet m : map) {
            result += m.toString() + ",";
        }
        return result;
    }
}
