/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package analysis.learning;

import java.util.ArrayList;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageMap {

 
    public ArrayList<ErlangCoverageMaplet> map;

    public ErlangCoverageMap() {
        map = new ArrayList<ErlangCoverageMaplet>();
    }

    @Override
    public String toString() {
        String result = "";
        for(ErlangCoverageMaplet m:map) {
            result += m.toString() + ",";
        }
        return result;
    }

}
