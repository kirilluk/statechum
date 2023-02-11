package statechum.analysis.learning.rpnicore;

import org.junit.Assert;
import org.junit.Test;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;
import statechum.Label;
import statechum.analysis.Erlang.TestErlangModule;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

public class TestParserDot {
    public static final Configuration config = Configuration.getDefaultConfiguration().copy();
    /** Label converter to use. */
    public final Transform.ConvertALabel converter = new Transform.InternStringLabel();
    @Test
    public final void testParseID1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a","graphname",config,graph,converter);
        Assert.assertEquals("a",parser.parseID());
    }

    @Test
    public final void testParseID1a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");
    }

    @Test
    public final void testParseID1b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a-","graphname",config,graph,converter);
        parser.parseText();
        Assert.assertEquals('-',parser.nextChar());
    }
    @Test
    public final void testParseID1c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("0","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseText();
        }},IllegalArgumentException.class,"ID cannot start with a digit");
    }
    @Test
    public final void testParseID1d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseText();
        }},IllegalArgumentException.class,"Invalid starting character for ID");
    }

    @Test
    public final void testParseID2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" ab ","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID4() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab;","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }
    @Test
    public final void testParseID5() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab\n","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID6() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("\"ab\"","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID7() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\" ","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID8a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\"\n","graphname",config,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID8b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\"b\\\"\"\n","graphname",config,graph,converter);
        Assert.assertEquals("a\"b\"",parser.parseID());
    }

    @Test
    public final void testParseID8c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\nb\\\n\"\n","graphname",config,graph,converter);
        Assert.assertEquals("a\nb\n",parser.parseID());
    }

    @Test
    public final void testParseID8d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\nb\\\n\"\n","graphname",config,graph,converter);
        Assert.assertEquals("a\nb\n",parser.parseID());
    }

    @Test
    public final void testParseID8e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\\b\"\n","graphname",config,graph,converter);
        Assert.assertEquals("a\\b",parser.parseID());
    }

    @Test
    public final void testParseID8f() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");    }

    @Test
    public final void testParseID8g() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\Qb\"\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"Invalid escape character starting from");
    }

    @Test
    public final void testParseID8h() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");
    }

    @Test
    public final void testParseID8i() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab;","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");
    }

    @Test
    public final void testParseID8j() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"string cannot contain a newline starting from");
    }

    @Test
    public final void testParseID9a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4","graphname",config,graph,converter);
        Assert.assertEquals("4",parser.parseID());
    }

    @Test
    public final void testParseID9b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4a","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"text character cannot be part of a number");
    }

    @Test
    public final void testParseID9c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4-","graphname",config,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('-',parser.nextChar());
    }

    @Test
    public final void testParseID9d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4+","graphname",config,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('+',parser.nextChar());
    }
    @Test
    public final void testParseID9e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" +4","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"invalid character");
    }

    @Test
    public final void testParseID9f() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4\n","graphname",config,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('\n',parser.nextChar());
    }


    @Test
    public final void testParseID10() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 45","graphname",config,graph,converter);
        Assert.assertEquals("45",parser.parseID());
    }

    @Test
    public final void testParseID11() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("4.5","graphname",config,graph,converter);
        Assert.assertEquals("4.5",parser.parseID());
    }

    @Test
    public final void testParseID12a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909","graphname",config,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }

    @Test
    public final void testParseID12b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.9.09","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"multiple dots in a number");
    }


    @Test
    public final void testParseID13() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-4","graphname",config,graph,converter);
        Assert.assertEquals("-4",parser.parseID());
    }

    @Test
    public final void testParseID14() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-4.8","graphname",config,graph,converter);
        Assert.assertEquals("-4.8",parser.parseID());
    }

    @Test
    public final void testParseID15a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-.8","graphname",config,graph,converter);
        Assert.assertEquals("-0.8",parser.parseID());
    }
    @Test
    public final void testParseID15b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-.","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");
    }
    @Test
    public final void testParseID15c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-. ","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"invalid number: should contain a digit");
    }
    @Test
    public final void testParseID16a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(".8","graphname",config,graph,converter);
        Assert.assertEquals("0.8",parser.parseID());
    }
    @Test
    public final void testParseID16b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(".","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"attempting to get next token");
    }
    @Test
    public final void testParseID16c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(". ","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseID();
        }},IllegalArgumentException.class,"invalid number: should contain a digit");
    }

    @Test
    public final void testParseID17a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[]","graphname",config,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }
    @Test
    public final void testParseID17b1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[;]","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseOptions();
        }},IllegalArgumentException.class,"invalid character starting from ;");
    }
    @Test
    public final void testParseID17b2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseOptions();
        }},IllegalArgumentException.class,"attempting to get next token");
    }
    @Test
    public final void testParseID17b3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseOptions();
        }},IllegalArgumentException.class,"attempting to get next token");
    }
    @Test
    public final void testParseID17c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n]","graphname",config,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }
    @Test
    public final void testParseID17d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17f() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;a=9]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }
    @Test
    public final void testParseID17g() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;c=\"db\"]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseID17h() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\nc=\"db\"]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParse1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a {  }","graphname",config,graph,converter);
        parser.parseGraph();
        Assert.assertTrue(graph.transitionMatrix.isEmpty());
    }

    @Test
    public final void testParse2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;a->b[label=lbl]; }","graphname",config,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse2", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=lbl];b->c[label=q]; }","graphname",config,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-q->c","testParse3", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse4() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c;__start0->a; }","graphname",config,graph,converter);
        Helper.checkForCorrectException(new Helper.whatToRun() { public @Override void run() {
            parser.parseGraph();
        }},IllegalArgumentException.class,"missing label option");
    }
    @Test
    public final void testParse5() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; }","graphname",config,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void runDots() throws IOException {

        String m49 = statechum.analysis.Erlang.TestErlangModule.loadFile(new File("D:\\experiment\\Frits\\m49.dot"));
        LearnerGraph reference = FsmParserDot.buildLearnerGraph(m49,"m49",config,converter);
        System.out.println(reference.getAcceptStateNumber());
        String hypothesis = statechum.analysis.Erlang.TestErlangModule.loadFile(new File("D:\\experiment\\Frits\\hypothesis_65.dot"));
        LearnerGraph hyp = FsmParserDot.buildLearnerGraph(hypothesis,"hypothesis",config,converter);

        Collection<List<Label>> ts = hyp.wmethod.getFullTestSet(2);
        System.out.println(ts.size());
        for(List<Label> seq:ts) {
            int hyp_value = hyp.paths.tracePathPrefixClosed(seq);
            int ref_value = reference.paths.tracePathPrefixClosed(seq);
            if (hyp_value != ref_value)
                System.out.println(Integer.toString(hyp_value)+" [hyp] "+Integer.toString(ref_value)+" [ref] " + seq);
        }
    }
}

