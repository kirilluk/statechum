/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package statechum;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/** This is a tree structure that contains traces but is sorted by prefix to allow quick prefix searching
 *
 * @author ramsay
 */
public class PrefixTraceTree {

    protected Trace prefix;
    protected ArrayList<PrefixTraceTree> children;
    protected boolean negative;

    public PrefixTraceTree() {
        this(new Trace());
    }

    public PrefixTraceTree(Trace t) {
        prefix = t;
        children = new ArrayList<PrefixTraceTree>();
    }

    /** Returns true if this is a prefix of t */
    public boolean isPrefix(Trace t) {
        return prefix.isPrefix(t);
    }

    public PrefixTraceTree(String filename) {
        this(new Trace());
        System.out.println("Loading PrefixTraceTree from " + filename + "...");
        BufferedReader input = null;
        try {
            input = new BufferedReader(new FileReader(filename));
            String line;
            while ((line = input.readLine()) != null) {
                line = line.trim();
                if (line.length() > 0) {
                    String neg = line.substring(0, 1);
                    String traceString = line.substring(1).trim();
                    Trace traceFromFile;
                    if (traceString.equals("")) {
                        traceFromFile = new Trace();
                    } else {
                        // FIXME needs Trace parser fix...
                        //traceFromFile = new Trace(QSMTool.tokeniseInput(traceString));
                        traceFromFile = new Trace();
                    }
                    if (neg.equals("-")) {
                        traceFromFile.negative = true;
                    }
                    this.add(traceFromFile);
                }
            }
        } catch (FileNotFoundException e) {
            System.out.println("Couldn't open trace file " + filename);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (input != null) { try { input.close();input=null; } catch(IOException toBeIgnored) { /* Ignore exception */ } }
        }
    }

    public void add(Trace t) {
        if (prefix.equals(t)) {
            // We really shouldn't get repeats...
            //System.out.println("******* Repeat " + t.toString() + " ********");
            return;
        } else {
            if (!this.isPrefix(t)) {
                throw new RuntimeException("Erroneous PrefixTree add!!");
            }
            boolean found = false;
            for (PrefixTraceTree c : children) {
                if (c.isPrefix(t)) {
                    c.add(t);
                    found = true;
                    break;
                }
            }
            if (!found) {
                children.add(new PrefixTraceTree(t));
            }
        }
    }

    public Trace findPrefix(Trace t) {
        if (this.isPrefix(t)) {
            //System.out.println(t.toString() + " >>> " + prefix.toString());
            // Maybe our children are longer
            Trace best = prefix;
            for (PrefixTraceTree tr : children) {
                Trace c = tr.findPrefix(t);
                if (c != null) {
                    //System.out.flush();
                    //System.out.println(tr.prefix.toString() + " <<< " + c.toString());
                    if (c.size() > best.size()) {
                        best = c;
                    }
                }
            }
            return best;
        } else {
            //System.out.println(t.toString() + " !!! " + prefix.toString());
            return null;
        }
    }

    @Override
    public String toString() {
        String result = prefix.toString();
        for (PrefixTraceTree c : children) {
            String sub = c.toString();
            sub = sub.replaceAll("\n", "\n\t");
            result += "\n\t" + sub;
        }
        return result;
    }
}
