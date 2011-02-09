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
public class CodeCoverageMap {

    public ArrayList<CodeCoverageMaplet> map;

    public void add(int line, int count) {
        map.add(new CodeCoverageMaplet(line, count));
    }

    public CodeCoverageMap() {
        map = new ArrayList<CodeCoverageMaplet>();
    }

    public CodeCoverageMap(CodeCoverageMap m) {
        map = (ArrayList<CodeCoverageMaplet>) m.map.clone();
    }

    @Override
    public String toString() {
        String result = "";
        for (CodeCoverageMaplet m : map) {
            result += m.toString() + ",";
        }
        return result;
    }

    public int findLine(int l) throws CodeCoverageMapletNotFoundException {
        for (CodeCoverageMaplet m : map) {
            if (m.line == l) {
                return m.count;
            }
        }
        throw new CodeCoverageMapletNotFoundException();
    }

    public void remove(int line) {
        ArrayList<CodeCoverageMaplet> removes = new ArrayList<CodeCoverageMaplet>();
        for (CodeCoverageMaplet m : map) {
            if (m.line == line) {
                removes.add(m);
            }
        }
        this.map.removeAll(removes);

    }

    public CodeCoverageMap subtract(CodeCoverageMap map2) {
        CodeCoverageMap result = new CodeCoverageMap();

        for (CodeCoverageMaplet m : map) {
            try {
                int c2 = map2.findLine(m.line);
                int count = m.count - c2;
                if (count > 0) {
                    result.add(m.line, count);
                }
            } catch (CodeCoverageMapletNotFoundException e) {
                // Not found in the subtraction so add it all
                result.add(m.line, m.count);
            }
        }

        return result;
    }

    public CodeCoverageMap disjunction(CodeCoverageMap map2) {
        CodeCoverageMap result = new CodeCoverageMap(this);

        for (CodeCoverageMaplet m : map2.map) {
            result.remove(m.line);
        }

        return result;
    }

    public CodeCoverageMapCombination intersection(CodeCoverageMap map2) {
        CodeCoverageMapCombination result = new CodeCoverageMapCombination();

        for (CodeCoverageMaplet m : map) {
            try {
                result.map.add(new CodeCoverageCombinedMaplet(m.line, m.count, map2.findLine(m.line)));
            } catch (CodeCoverageMapletNotFoundException e) {
                ;
            }
        }
        return result;
    }
}
