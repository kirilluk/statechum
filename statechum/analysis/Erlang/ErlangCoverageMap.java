/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum.analysis.Erlang;

import java.util.ArrayList;

/**
 *
 * @author ramsay
 */
public class ErlangCoverageMap {

    public ArrayList<ErlangCoverageMaplet> map;

    public void add(int line, int count) {
        map.add(new ErlangCoverageMaplet(line, count));
    }

    public ErlangCoverageMap() {
        map = new ArrayList<ErlangCoverageMaplet>();
    }

    public ErlangCoverageMap(ErlangCoverageMap m) {
        map = (ArrayList<ErlangCoverageMaplet>) m.map.clone();
    }

    @Override
    public String toString() {
        String result = "";
        for (ErlangCoverageMaplet m : map) {
            result += m.toString() + ",";
        }
        return result;
    }

    public int findLine(int l) throws ErlangCoverageMapletNotFoundException {
        for (ErlangCoverageMaplet m : map) {
            if (m.line == l) {
                return m.count;
            }
        }
        throw new ErlangCoverageMapletNotFoundException();
    }

    public void remove(int line) {
        ArrayList<ErlangCoverageMaplet> removes = new ArrayList<ErlangCoverageMaplet>();
        for (ErlangCoverageMaplet m : map) {
            if (m.line == line) {
                removes.add(m);
            }
        }
        this.map.removeAll(removes);

    }

    public ErlangCoverageMap subtract(ErlangCoverageMap map2) {
        ErlangCoverageMap result = new ErlangCoverageMap();

        for (ErlangCoverageMaplet m : map) {
            try {
                int c2 = map2.findLine(m.line);
                int count = m.count - c2;
                if (count > 0) {
                    result.add(m.line, count);
                }
            } catch (ErlangCoverageMapletNotFoundException e) {
                // Not found in the subtraction so add it all
                result.add(m.line, m.count);
            }
        }

        return result;
    }

    public ErlangCoverageMap disjunction(ErlangCoverageMap map2) {
        ErlangCoverageMap result = new ErlangCoverageMap(this);

        for (ErlangCoverageMaplet m : map2.map) {
            result.remove(m.line);
        }

        return result;
    }

    public ErlangCoverageMapCombination intersection(ErlangCoverageMap map2) {
        ErlangCoverageMapCombination result = new ErlangCoverageMapCombination();

        for (ErlangCoverageMaplet m : map) {
            try {
                result.map.add(new ErlangCoverageCombinedMaplet(m.line, m.count, map2.findLine(m.line)));
            } catch (ErlangCoverageMapletNotFoundException e) {
                ;
            }
        }
        return result;
    }
}
