package statechum.analysis.learning.rpnicore;

import org.junit.Assert;
import org.junit.Test;
import statechum.Configuration;
import statechum.DeterministicDirectedSparseGraph;
import statechum.Helper;

import java.util.Map;

import static statechum.analysis.learning.rpnicore.FsmParser.buildLearnerGraph;

public class TestParserDot {
    public static final Configuration config = Configuration.getDefaultConfiguration().copy();
    /** Label converter to use. */
    public static final Transform.ConvertALabel converter = new Transform.InternStringLabel();
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
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
        Helper.checkForCorrectException(parser::parseText,IllegalArgumentException.class,"ID cannot start with a digit");
    }
    @Test
    public final void testParseID1d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseText,IllegalArgumentException.class,"Invalid starting character for ID");
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");    }

    @Test
    public final void testParseID8g() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\Qb\"\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Invalid escape character starting from");
    }

    @Test
    public final void testParseID8h() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParseID8i() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab;","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParseID8j() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"string cannot contain a newline starting from");
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"text character cannot be part of a number");
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid character");
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
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909 ","graphname",config,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }
    @Test
    public final void testParseID12c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909\n","graphname",config,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }
    @Test
    public final void testParseID12d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.9.09","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"multiple dots in a number");
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID15c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-. ","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid number: should contain a digit");
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
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID16c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(". ","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid number: should contain a digit");
    }

    @Test
    public final void testParseID17a1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[]","graphname",config,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }
    @Test
    public final void testParseID17a2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" ","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"options should begin with '['");
    }
    @Test
    public final void testParseID17b1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[;]","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }
    @Test
    public final void testParseID17b2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID17b3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\n","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID17c1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n]","graphname",config,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }

    @Test
    public final void testParseID17c2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[ ]","graphname",config,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }

    @Test
    public final void testParseID17c3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[;]","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }

    @Test
    public final void testParseID17c4() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n,]","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ,");
    }
    @Test
    public final void testParseID17d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b\n]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }

    @Test
    public final void testParseID17e3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e4() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b \n;]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e5() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b \n\t\t;]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e6() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ,\n ]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }

    @Test
    public final void testParseID17e7() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ,\n;\n ]","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }

    @Test
    public final void testParseID17f1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;a=9]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }
    @Test
    public final void testParseID17f2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b,a=9]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }
    @Test
    public final void testParseID17f3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b\na=9]","graphname",config,graph,converter);
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
    public final void testParseID17i() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n\ta=b;\nc=\"db\"]","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseID17j() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\nc=\"db\"\n\t]\n\n","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }
    @Test
    public final void testParseID17k() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a\n\t=b;c=db]\n\n","graphname",config,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }    @Test
    public final void testParseID17l() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=\n\tb;\nc=db]\n\n","graphname",config,graph,converter);
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
    public final void testParse3a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=lbl];b->c[label=q]; }","graphname",config,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-q->c","testParse3", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    /** quoted label name containing a newline. */
    @Test
    public final void testParse3b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=lbl];b->c[label=\"q\\\n\"]; }","graphname",config,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse3", config,converter);
        DeterministicDirectedSparseGraph.CmpVertex vert = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"), config);
        gr.transitionMatrix.put(vert, gr.createNewRow());
        gr.addTransition(
                gr.transitionMatrix.get(gr.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"))),
                AbstractLearnerGraph.generateNewLabel("q\n",gr.config,null),vert);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse4a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c;__start0->a; }","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse4b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;b->c; }","graphname",config,graph,converter);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse5a1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    /** Where the option __start0 is not given, it is not used to denote the initial state and thus transition to the initial state containing no label is flagged as erroneous. */
    @Test
    public final void testParse5a2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; }","graphname",config,graph,converter,false,false);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse5b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a\nb;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a\n;\tb;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a,b;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);// state names are different here but graph structure is isomorphic.
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5f() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b\n\tc;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5g() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7;b;-9;__start0\n-6.7->b[label=lbl];b->-9[label=\"u\"];__start0->-6.7\t\n }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5h() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7 -8 -9;__start0\n-6.7->-8[label=lbl];-8->-9[label=\"u\"];__start0->-6.7\t\n }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse5i() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0\na->b[label=lbl];b->c[label=\"u\"];__start0->a\t\n }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse6a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; __start0->a;}","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"multiple initial state");
    }
    @Test
    public final void testParse6b1() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { ab;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State a not defined");
    }
    @Test
    public final void testParse6b2() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->T[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State T not defined");
    }
    @Test
    public final void testParse6b3() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\ta;__start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State a already defined");
    }
    @Test
    public final void testParse6c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[lBabel=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse6d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b--c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"arrow should end with '>'");
    }
    @Test
    public final void testParse7a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse8a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("graph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"The graph should be labelled as directed graph");
    }

    @Test
    public final void testParse8b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid character");
    }

    @Test
    public final void testParse8c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a  a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"The graph description should be enclosed in curly braces");
    }

    @Test
    public final void testParse8d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; ","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParse8e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; } junk","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Extra text at the end of graph");
    }

    @Test
    public final void testParse9a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[label=1];b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());
    }

    @Test
    public final void testParse9b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[label=1];b;c;__start0[label=no_point_labelling_start0];a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());
    }

    @Test
    public final void testParse9c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[abel=3];b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse9d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label[=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"option label should have a value");
    }
    @Test
    public final void testParse9e() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=[lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid character");
    }
    @Test
    public final void testParse9f() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl[];b->c[label=u] __start0->a; }","graphname",config,graph,converter,false,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid character");
    }

    @Test
    public final void testParse9g() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7[label=M] -8[label=N] -9[label=R];__start0\n-6.7->-8[label=lbl];-8->-9[label=\"u\"];__start0->-6.7\t\n }","graphname",config,graph,converter,false,true);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", config,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("M",graph.getInit().getStringId());
        Assert.assertEquals("N",graph.getTransitionMatrix().get(graph.getInit()).get(AbstractLearnerGraph.generateNewLabel("lbl",gr.config,null)).getStringId());
        Assert.assertEquals("R",graph.getTransitionMatrix().get(graph.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("N"))).get(AbstractLearnerGraph.generateNewLabel("u",gr.config,null)).getStringId());
    }

    @Test
    public final void testParse10a() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }","graphname",config,graph,converter,true,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }
    @Test
    public final void testParse10b() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/t/r\"];b->c[label=\"u/p\"] __start0->a; }","graphname",config,graph,converter,true,true);
        Helper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }
    /** Tests that transitions with input/output pairs are handled as atomic labels. */
    public final LearnerGraph constructReferenceGraphWithErrorTransition(String label) {
        LearnerGraph gr = new LearnerGraph(config);gr.initEmpty();
        DeterministicDirectedSparseGraph.CmpVertex a = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"), config);
        gr.transitionMatrix.put(a, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex b = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"), config);
        gr.transitionMatrix.put(b, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex c = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"), config);
        gr.transitionMatrix.put(c, gr.createNewRow());
        gr.addTransition(gr.transitionMatrix.get(a),AbstractLearnerGraph.generateNewLabel("lbl/g",gr.config,null),b);
        gr.addTransition(gr.transitionMatrix.get(b),AbstractLearnerGraph.generateNewLabel(label,gr.config,null),c);
        return gr;
    }
    public final LearnerGraph constructReferenceGraphNoErrorTransition() {
        LearnerGraph gr = new LearnerGraph(config);gr.initEmpty();
        DeterministicDirectedSparseGraph.CmpVertex a = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"), config);
        gr.transitionMatrix.put(a, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex b = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"), config);
        gr.transitionMatrix.put(b, gr.createNewRow());
        gr.addTransition(gr.transitionMatrix.get(a),AbstractLearnerGraph.generateNewLabel("lbl/g",gr.config,null),b);
        return gr;
    }
    @Test
    public final void testParse10c() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }","graphname",config,graph,converter,true,true);
        parser.parseGraph();
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p");
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }


    /** This test ensures that transitions with output 'error' are not added to the transition matrix at all. */
    @Test
    public final void testParse10d() {
        LearnerGraph graph = new LearnerGraph(config);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }","graphname",config,graph,converter,true,true);
        parser.parseGraph();
        LearnerGraph gr = constructReferenceGraphNoErrorTransition();
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11a() {
        LearnerGraph gr = constructReferenceGraphNoErrorTransition();
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }","graphA",config,null, true);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }
    @Test
    public final void testParse11b() {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/error");
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }","graphA",config,null, false);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }
    @Test
    public final void testParse11c() {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p");
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }","graphA",config,null, false);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11d() throws AMEquivalenceClass.IncompatibleStatesException {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p");
        LearnerGraphND graphND = FsmParserDot.buildLearnerGraphND("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }","graphA",config,null, false);
        LearnerGraph graph = graphND.pathroutines.buildDeterministicGraph();
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

}

