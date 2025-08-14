package statechum.analysis.learning.rpnicore;

import org.junit.Assert;
import org.junit.Test;
import statechum.*;
import statechum.analysis.learning.util.OutputUtil;

import java.util.*;

import static statechum.analysis.learning.rpnicore.FsmParserStatechum.buildLearnerGraph;

public class TestParserDot {
    public static final Configuration configLTS = Configuration.getDefaultConfiguration().copy();
    public static final Configuration configMealy = Configuration.getDefaultConfiguration().copy();
    public static final Configuration configAtomicPairs = Configuration.getDefaultConfiguration().copy();
    static {
        configMealy.setLabelKind(Configuration.LABELKIND.LABEL_INPUT_OUTPUT);
        configLTS.setLabelKind(Configuration.LABELKIND.LABEL_STRING);
        configAtomicPairs.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
    }
    /** Label converter to use. */
    public static final Transform.ConvertALabel converter = new Transform.InternStringLabel();
    @Test
    public final void testParseID1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a", configLTS,graph,converter);
        Assert.assertEquals("a",parser.parseID());
    }

    @Test
    public final void testParseID1a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParseID1b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a-", configLTS,graph,converter);
        parser.parseText(false);
        Assert.assertEquals('-',parser.nextChar());
    }
    @Test
    public final void testParseID1c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("0", configLTS,graph,converter);
        TestHelper.checkForCorrectException(() -> parser.parseText(false),IllegalArgumentException.class,"ID cannot start with a digit");
    }
    @Test
    public final void testParseID1d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-", configLTS,graph,converter);
        TestHelper.checkForCorrectException(() -> parser.parseText(false),IllegalArgumentException.class,"Invalid starting character for ID");
    }
    @Test
    public final void testParseID1e() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a=b", configLTS,graph,converter);
        Assert.assertEquals("a",parser.parseText(false));
        Assert.assertEquals('=',parser.nextChar());
    }
    @Test
    public final void testParseID1f() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("a=b", configLTS,graph,converter);
        Assert.assertEquals("a=b",parser.parseText(true));
    }
    @Test
    public final void testParseID2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" ab ", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID4() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab;", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }
    @Test
    public final void testParseID5() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("ab\n", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID6() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("\"ab\"", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID7() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\" ", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID8a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\"\n", configLTS,graph,converter);
        Assert.assertEquals("ab",parser.parseID());
    }

    @Test
    public final void testParseID8b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\"b\\\"\"\n", configLTS,graph,converter);
        Assert.assertEquals("a\"b\"",parser.parseID());
    }

    @Test
    public final void testParseID8c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\nb\\\n\"\n", configLTS,graph,converter);
        Assert.assertEquals("a\nb\n",parser.parseID());
    }

    @Test
    public final void testParseID8d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\nb\\\n\"\n", configLTS,graph,converter);
        Assert.assertEquals("a\nb\n",parser.parseID());
    }

    @Test
    public final void testParseID8e() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\\\b\"\n", configLTS,graph,converter);
        Assert.assertEquals("a\\b",parser.parseID());
    }

    @Test
    public final void testParseID8f() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");    }

    @Test
    public final void testParseID8g() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"a\\Qb\"\n", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Invalid escape character starting from");
    }

    @Test
    public final void testParseID8h() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParseID8i() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab;", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParseID8j() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" \"ab\n", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"string cannot contain a newline starting from");
    }

    @Test
    public final void testParseID9a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4", configLTS,graph,converter);
        Assert.assertEquals("4",parser.parseID());
    }

    @Test
    public final void testParseID9b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4a", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"text character cannot be part of a number");
    }

    @Test
    public final void testParseID9c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4-", configLTS,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('-',parser.nextChar());
    }

    @Test
    public final void testParseID9d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4+", configLTS,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('+',parser.nextChar());
    }
    @Test
    public final void testParseID9e() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" +4", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid character");
    }

    @Test
    public final void testParseID9f() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 4\n", configLTS,graph,converter);
        Assert.assertEquals("4",parser.parseID());
        Assert.assertEquals('\n',parser.nextChar());
    }


    @Test
    public final void testParseID10() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" 45", configLTS,graph,converter);
        Assert.assertEquals("45",parser.parseID());
    }

    @Test
    public final void testParseID11() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("4.5", configLTS,graph,converter);
        Assert.assertEquals("4.5",parser.parseID());
    }

    @Test
    public final void testParseID12a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909", configLTS,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }

    @Test
    public final void testParseID12b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909 ", configLTS,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }
    @Test
    public final void testParseID12c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.909\n", configLTS,graph,converter);
        Assert.assertEquals("43.909",parser.parseID());
    }
    @Test
    public final void testParseID12d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("43.9.09", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"multiple dots in a number");
    }

    @Test
    public final void testParseID13() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-4", configLTS,graph,converter);
        Assert.assertEquals("-4",parser.parseID());
    }

    @Test
    public final void testParseID14() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-4.8", configLTS,graph,converter);
        Assert.assertEquals("-4.8",parser.parseID());
    }

    @Test
    public final void testParseID15a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-.8", configLTS,graph,converter);
        Assert.assertEquals("-0.8",parser.parseID());
    }
    @Test
    public final void testParseID15b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-.", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID15c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("-. ", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid number: should contain a digit");
    }
    @Test
    public final void testParseID16a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(".8", configLTS,graph,converter);
        Assert.assertEquals("0.8",parser.parseID());
    }
    @Test
    public final void testParseID16b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(".", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID16c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(". ", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseID,IllegalArgumentException.class,"invalid number: should contain a digit");
    }

    @Test
    public final void testParseID17a1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[]", configLTS,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }
    @Test
    public final void testParseID17a2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>(" ", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"options should begin with '['");
    }
    @Test
    public final void testParseID17b1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[;]", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }
    @Test
    public final void testParseID17b2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID17b3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\n", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"Premature end of input");
    }
    @Test
    public final void testParseID17c1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n]", configLTS,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }

    @Test
    public final void testParseID17c2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[ ]", configLTS,graph,converter);
        Assert.assertTrue(parser.parseOptions().isEmpty());
    }

    @Test
    public final void testParseID17c3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[;]", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }

    @Test
    public final void testParseID17c4() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n,]", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ,");
    }
    @Test
    public final void testParseID17d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b\n]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }

    @Test
    public final void testParseID17e3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e4() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b \n;]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e5() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b \n\t\t;]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }
    @Test
    public final void testParseID17e6() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ,\n ]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b}",options.toString());
    }

    @Test
    public final void testParseID17e7() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b ,\n;\n ]", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseOptions,IllegalArgumentException.class,"invalid character starting from ;");
    }

    @Test
    public final void testParseID17f1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;a=9]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }
    @Test
    public final void testParseID17f2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b,a=9]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }
    @Test
    public final void testParseID17f3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b\na=9]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=9}",options.toString());
    }

    @Test
    public final void testParseID17g() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;c=\"db\"]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseID17h() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\nc=\"db\"]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseID17i() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[\n\ta=b;\nc=\"db\"]", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseID17j() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=b;\nc=\"db\"\n\t]\n\n", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }
    @Test
    public final void testParseID17k() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a\n\t=b;c=db]\n\n", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }    @Test
    public final void testParseID17l() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("[a=\n\tb;\nc=db]\n\n", configLTS,graph,converter);
        Map<String,String> options = parser.parseOptions();
        Assert.assertEquals("{a=b, c=db}",options.toString());
    }

    @Test
    public final void testParseLabel1() {
        LabelInputOutput a=new LabelInputOutput("in1/out",true,false);
        Assert.assertEquals("in1",a.input);
        Assert.assertEquals("out",a.output);
        Assert.assertFalse(a.errorTransition);
    }

    @Test
    public final void testParseLabel2a() {
        LabelInputOutput a=new LabelInputOutput("in1/error",true,false);
        Assert.assertEquals("in1",a.input);
        Assert.assertEquals(LabelInputOutput.ERROR,a.output);
        Assert.assertTrue(a.errorTransition);
    }

    @Test
    public final void testParseLabel2b() {
        LabelInputOutput a=new LabelInputOutput("in1/Error",true,false);
        Assert.assertEquals("in1",a.input);
        Assert.assertEquals(LabelInputOutput.ERROR,a.output);
        Assert.assertTrue(a.errorTransition);
    }

    @Test
    public final void testParseLabel3() {
        LabelInputOutput a=new LabelInputOutput("in1/eRror",false,false);
        Assert.assertEquals("in1",a.input);
        Assert.assertEquals("eRror",a.output);
        Assert.assertFalse(a.errorTransition);
    }

    @Test
    public final void testParseLabel4a() {
        TestHelper.checkForCorrectException(()-> new LabelInputOutput("in1",true,false),IllegalArgumentException.class,"invalid format of label ");
    }

    @Test
    public final void testParseLabel4b() {
        TestHelper.checkForCorrectException(()-> new LabelInputOutput("in1/b/c",true,false),IllegalArgumentException.class,"invalid format of label ");
    }

    @Test
    public final void testParseLabel5() {
        LabelInputOutput a=new LabelInputOutput("in1/No_out",false,false);
        Assert.assertEquals("in1",a.input);
        Assert.assertEquals(LabelInputOutput.NO_OUTPUT,a.output);
        Assert.assertFalse(a.errorTransition);
    }

    @Test
    public final void testIOLabel1() {
        LabelInputOutput a=new LabelInputOutput("in1/out",true,false);
        LabelInputOutput b=new LabelInputOutput("in2/out",true,false);
        Assert.assertNotEquals(a,b);
    }

    @Test
    public final void testIOLabel2() {
        LabelInputOutput a=new LabelInputOutput("in1/out1",true,false);
        LabelInputOutput b=new LabelInputOutput("in1/out2",true,false);
        Assert.assertEquals(a,b);
    }

    @Test
    public final void testIOLabel3() {
        LabelInputOutput a=new LabelInputOutput("in1/out",true,true);
        LabelInputOutput b=new LabelInputOutput("in2/out",true,true);
        Assert.assertNotEquals(a,b);
    }

    @Test
    public final void testIOLabel4() {
        LabelInputOutput a=new LabelInputOutput("in1/out1",true,true);
        LabelInputOutput b=new LabelInputOutput("in1/out1",true,true);
        Assert.assertEquals(a,b);
    }

    @Test
    public final void testIOLabel5() {
        LabelInputOutput a=new LabelInputOutput("in1/out1",true,true);
        LabelInputOutput b=new LabelInputOutput("in1/out2",true,true);
        Assert.assertNotEquals(a,b);
    }

    @Test
    public final void testParse1a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a {  }", configLTS,graph,converter);
        parser.parseGraph();
        Assert.assertTrue(graph.transitionMatrix.isEmpty());
    }

    @Test
    public final void testParse1b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph {  }", configLTS,graph,converter);
        parser.parseGraph();
        Assert.assertTrue(graph.transitionMatrix.isEmpty());
    }

    @Test
    public final void testParse2a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;a->b[label=lbl]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse2", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse2b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph { a;b;a->b[label=lbl]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse2", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    // Tests both accept and reject-states
    public final void testParse2c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[shape=circle];b[shape=square];a->b[label=lbl]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl-#b","testParse2", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertTrue(gr.transitionMatrix.findKey(DeterministicDirectedSparseGraph.VertexID.parseID("a")).isAccept());
        Assert.assertFalse(gr.transitionMatrix.findKey(DeterministicDirectedSparseGraph.VertexID.parseID("b")).isAccept());
    }
    @Test
    // Tests elements of dot format that I'm ignoring such as properties at graph level and nodes called 'node'.
    public final void testParse2d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { p=q;a;t=r;node[label=unknown];b;a->b[label=lbl]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse2", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse3a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=lbl];b->c[label=q]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-q->c","testParse3", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    /** quoted label name containing a newline. */
    @Test
    public final void testParse3b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=lbl];b->c[label=\"q\\\n\"]; }", configLTS,graph,converter);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b","testParse3", configLTS,converter);
        DeterministicDirectedSparseGraph.CmpVertex vert = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"), configLTS);
        gr.transitionMatrix.put(vert, gr.createNewRow());
        gr.addTransition(
                gr.transitionMatrix.get(gr.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"))),
                AbstractLearnerGraph.generateNewLabel("q\n",gr.config,null),vert);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse3c() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=\"in1/out1\"];b->c[label=\"in2/out2\"]; }", configMealy,graph,null);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-in1/out1->b | b-in2/out2->c","testParse3", configMealy,null);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse3d() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=\"in1/error\"];b->c[label=\"in2/error\"]; }",
                configMealy,graph,null,true, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND);
        parser.parseGraph();
        Assert.assertNotNull(graph.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a")));
        Assert.assertTrue(graph.transitionMatrix.get(graph.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"))).isEmpty());
        Assert.assertEquals(Arrays.asList(new LabelInputOutput("in1/error",true,false),new LabelInputOutput("in2/error",true,false)),
                new LinkedList<>(graph.pathroutines.computeAlphabet()));
    }

    @Test
    public final void testParse3e() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;a->b[label=\"in1/out1\"];b->c[label=\"in2/error\"]; }",
                configMealy,graph,null,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-in1/out1->b","testParse3", configMealy,null);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.findVertex("a"), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals(new TreeSet<Label>(Arrays.asList(new LabelInputOutput("in1/out1",true,false),new LabelInputOutput("in2/error",true,false))),
                graph.pathroutines.computeAlphabet());
    }
    @Test
    public final void testParse4a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c;__start0->a; }", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse4b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;b->c; }", configLTS,graph,converter);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse5a1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    /** Where the option __start0 is not given, it is not used to denote the initial state and thus transition to the initial state containing no label is flagged as erroneous. */
    @Test
    public final void testParse5a2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; }",
                configLTS,graph,converter,false, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse5b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false, FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a\nb;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a\n;\tb;c;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5e() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a,b;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);// state names are different here but graph structure is isomorphic.
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5f() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b\n\tc;__start0;a->b[label=lbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5g() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7;b;-9;__start0\n-6.7->b[label=lbl];b->-9[label=\"u\"];__start0->-6.7\t\n }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse5h() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7 -8 -9;__start0\n-6.7->-8[label=lbl];-8->-9[label=\"u\"];__start0->-6.7\t\n }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse5i() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0\na->b[label=lbl];b->c[label=\"u\"];__start0->a\t\n }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse6a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=\"u\"];__start0->a; __start0->a;}",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"multiple initial state");
    }
    @Test
    public final void testParse6b1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { ab;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State a not defined");
    }
    @Test
    public final void testParse6b2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->T[label\t=\tlbl];b->c[label=\"u\"]\n\t__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State T not defined");
    }
    @Test
    public final void testParse6b3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label\t=\tlbl];b->c[label=\"u\"]\n\ta;__start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"State a already defined");
    }
    @Test
    public final void testParse6c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[lBabel=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Missing label option");
    }

    @Test
    public final void testParse6d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b--c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"arrow should end with '>'");
    }
    @Test
    public final void testParse7a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse8a() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("graph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"The graph should be labelled as directed graph");
    }

    @Test
    public final void testParse8b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a  a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"The graph description should be enclosed in curly braces");
    }

    @Test
    public final void testParse8c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; ",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Premature end of input");
    }

    @Test
    public final void testParse8d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; } junk",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"Extra text at the end of graph");
    }

    @Test
    public final void testParse9a1() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[label=1];b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());
    }

    @Test
    public final void testParse9a2() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { b;a[label=1];c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());
    }

    @Test
    public final void testParse9a3() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[label=1];b;c;a->b[label=lbl];b->c[label=u]; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());
    }

    @Test
    public final void testParse9b() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[label=1];b;c;__start0[label=no_point_labelling_start0];a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("1",graph.getInit().getStringId());

        Set<DeterministicDirectedSparseGraph.CmpVertex> expectedStates = new TreeSet<>();
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("1"),graph.config));
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"),graph.config));
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"),graph.config));
        Assert.assertEquals(expectedStates,graph.transitionMatrix.keySet());
    }

    @Test
    public final void testParse9c() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a[abel=3];b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        Set<DeterministicDirectedSparseGraph.CmpVertex> expectedStates = new TreeSet<>();
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"),graph.config));
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"),graph.config));
        expectedStates.add(AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"),graph.config));
        Assert.assertEquals(expectedStates,graph.transitionMatrix.keySet());
    }

    @Test
    public final void testParse9d() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label[=lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"option label should have a value");
    }
    @Test
    public final void testParse9e() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=[lbl];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid character");
    }
    @Test
    public final void testParse9f() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl[];b->c[label=u] __start0->a; }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid character");
    }

    @Test
    public final void testParse9g() {
        LearnerGraph graph = new LearnerGraph(configLTS);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { -6.7[label=M] -8[label=N] -9[label=R];__start0\n-6.7->-8[label=lbl];-8->-9[label=\"u\"];__start0->-6.7\t\n }",
                configLTS,graph,converter,false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = buildLearnerGraph("a-lbl->b-u->c","testParse4", configLTS,converter);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("M",graph.getInit().getStringId());
        Assert.assertEquals("N",graph.getTransitionMatrix().get(graph.getInit()).get(AbstractLearnerGraph.generateNewLabel("lbl",gr.config,null)).getStringId());
        Assert.assertEquals("R",graph.getTransitionMatrix().get(graph.findVertex(DeterministicDirectedSparseGraph.VertexID.parseID("N"))).get(AbstractLearnerGraph.generateNewLabel("u",gr.config,null)).getStringId());
    }

    @Test
    public final void testParse10a() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configMealy,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }
    @Test
    public final void testParse10b() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/t/r\"];b->c[label=\"u/p\"] __start0->a; }",
                configMealy,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }
    @Test
    public final void testParse10c() {
        LearnerGraph graph = new LearnerGraph(configAtomicPairs);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=lbl];b->c[label=u] __start0->a; }",
                configAtomicPairs,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }
    @Test
    public final void testParse10d() {
        LearnerGraph graph = new LearnerGraph(configAtomicPairs);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/t/r\"];b->c[label=\"u/p\"] __start0->a; }",
                configAtomicPairs,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(parser::parseGraph,IllegalArgumentException.class,"invalid format of label ");
    }

    /** Tests that transitions with input/output pairs are handled as atomic labels. */
    public final LearnerGraph constructReferenceGraphWithErrorTransition(String label, Configuration config) {
        LearnerGraph gr = new LearnerGraph(config);gr.initEmpty();
        DeterministicDirectedSparseGraph.CmpVertex a = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"), gr.config);
        gr.transitionMatrix.put(a, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex b = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"), gr.config);
        gr.transitionMatrix.put(b, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex c = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("c"), gr.config);
        gr.transitionMatrix.put(c, gr.createNewRow());
        gr.addTransition(gr.transitionMatrix.get(a),AbstractLearnerGraph.generateNewLabel("lbl/g",gr.config,null),b);
        gr.addTransition(gr.transitionMatrix.get(b),AbstractLearnerGraph.generateNewLabel(label,gr.config,null),c);
        gr.setName("Reference Graph");
        return gr;
    }
    public final LearnerGraph constructReferenceGraphNoErrorTransition() {
        LearnerGraph gr = new LearnerGraph(configMealy);gr.initEmpty();
        DeterministicDirectedSparseGraph.CmpVertex a = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("a"), gr.config);
        gr.transitionMatrix.put(a, gr.createNewRow());
        DeterministicDirectedSparseGraph.CmpVertex b = AbstractLearnerGraph.generateNewCmpVertex(DeterministicDirectedSparseGraph.VertexID.parseID("b"), gr.config);
        gr.transitionMatrix.put(b, gr.createNewRow());
        gr.addTransition(gr.transitionMatrix.get(a),AbstractLearnerGraph.generateNewLabel("lbl/g",gr.config,null),b);
        return gr;
    }

    @Test
    public final void testParse10e() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }",
                configMealy,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p", configMealy);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    /** This test ensures that transitions with output 'error' are not added to the transition matrix at all. */
    @Test
    public final void testParse10f() {
        LearnerGraph graph = new LearnerGraph(configMealy);graph.initEmpty();
        FsmParserDot<DeterministicDirectedSparseGraph.CmpVertex,LearnerGraphCachedData> parser = new FsmParserDot<>("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }",
                configMealy,graph,converter,true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        parser.parseGraph();
        LearnerGraph gr = constructReferenceGraphNoErrorTransition();
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11a() {
        LearnerGraph gr = constructReferenceGraphNoErrorTransition();
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }",
                configMealy,null, true,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11b() {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/error", configLTS);
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/error\"];__start0->a; }",
                configLTS,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11c() {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p", configLTS);
        LearnerGraph graph = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }",
                configLTS,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse11d() throws AMEquivalenceClass.IncompatibleStatesException {
        LearnerGraph gr = constructReferenceGraphWithErrorTransition("u/p", configLTS);
        LearnerGraphND graphND = FsmParserDot.buildLearnerGraphND("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];__start0->a; }",
                configLTS,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph graph = graphND.pathroutines.buildDeterministicGraph();
        Assert.assertNull(WMethod.checkM(gr, gr.findVertex("a"),graph,graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        Assert.assertEquals("a",graph.getInit().getStringId());
    }

    @Test
    public final void testParse12a() {
        TestHelper.checkForCorrectException(() -> FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"lbl/k\"];__start0->a; }",
                configMealy,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0),IllegalArgumentException.class,"non-determinism detected from state a for transition lbl/k to state c");
    }

    @Test
    public final void testParse12b() {
        LearnerGraph gr = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"lbl/k\"];__start0->a; }",
                configAtomicPairs,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        TestHelper.checkForCorrectException(gr.transform::convertIO,IllegalArgumentException.class,"non-determinism detected for input lbl/k to state c");
    }

    @Test
    public final void testParse13a1() {
        LearnerGraph grM = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];__start0->a; }",
                configMealy,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph gr = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];__start0->a; }",
                configAtomicPairs,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph grConf = gr.transform.convertIO();
        Assert.assertNull(WMethod.checkM(grM, grM.getInit(),grConf,grConf.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        LearnerGraph grBackToPairs = grConf.transform.convertToIOPairs();
        Assert.assertNull(WMethod.checkM(grBackToPairs, grBackToPairs.getInit(),gr,gr.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    // Tests that transition to reject-state is ignored by the pairs->mealy translation
    public final void testParse13a2() {
        LearnerGraph grM = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];__start0->a; }",
                configMealy,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph gr = FsmParserDot.buildLearnerGraph("digraph a { a;b;r[shape=square];c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];b->r[label=\"u/g\"];a->c[label=\"u/k\"];__start0->a; }",
                configAtomicPairs,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph grConf = gr.transform.convertIO();
        Assert.assertNull(WMethod.checkM(grM, grM.getInit(),grConf,grConf.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        LearnerGraph grBackToPairs = grConf.transform.convertToIOPairs();
        // grBackToPairs will not have a reject-state thus we add it.
        DeterministicDirectedSparseGraph.CmpVertex rejectState = AbstractLearnerGraph.generateNewCmpVertex(grBackToPairs.nextID(false), grBackToPairs.config);rejectState.setAccept(false);
        grBackToPairs.transitionMatrix.put(rejectState,grBackToPairs.createNewRow());
        grBackToPairs.addTransition(grBackToPairs.transitionMatrix.get(DeterministicDirectedSparseGraph.VertexID.parseID("b")),
            AbstractLearnerGraph.generateNewLabel("u/g",grBackToPairs.config,null),rejectState);
        Assert.assertNull(WMethod.checkM(grBackToPairs, grBackToPairs.getInit(),gr,gr.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
    @Test
    public final void testParse13b() {
        LearnerGraph grM = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];c->a[label=\"lbl/k\"];__start0->a; }",
                configMealy,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph gr = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];c->a[label=\"lbl/k\"];__start0->a; }",
                configAtomicPairs,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph grConf = gr.transform.convertIO();
        Assert.assertNull(WMethod.checkM(grM, grM.getInit(),grConf,grConf.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        LearnerGraph grBackToPairs = grConf.transform.convertToIOPairs();
        Assert.assertNull(WMethod.checkM(grBackToPairs, grBackToPairs.getInit(),gr,gr.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testParse13c() {
        LearnerGraph grM = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;a->b[label=\"lbl/g\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];c->a[label=\"lbl/k\"];__start0->a; }",
                configMealy,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        LearnerGraph gr = FsmParserDot.buildLearnerGraph("digraph a { a;b;c;__start0;r;a->b[label=\"lbl/g\"];a->r[label=\"lbl/t\"];a->r[label=\"lbl/p\"];b->c[label=\"u/p\"];a->c[label=\"u/k\"];c->r[label=\"lbl/g\"];c->a[label=\"lbl/k\"];__start0->a; }",
                configAtomicPairs,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.USE_START0);
        gr.findVertex("r").setAccept(false);// make r the reject state
        LearnerGraph grConf = gr.transform.convertIO();
        Assert.assertNull(WMethod.checkM(grM, grM.getInit(),grConf,grConf.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
        LearnerGraph grBackToPairs = grConf.transform.convertToIOPairs();
        // grBackToPairs will not have a reject-state thus we add it.
        DeterministicDirectedSparseGraph.CmpVertex rejectState = AbstractLearnerGraph.generateNewCmpVertex(grBackToPairs.nextID(false), grBackToPairs.config);rejectState.setAccept(false);
        grBackToPairs.transitionMatrix.put(rejectState,grBackToPairs.createNewRow());
        grBackToPairs.addTransition(grBackToPairs.transitionMatrix.get(DeterministicDirectedSparseGraph.VertexID.parseID("a")),
                AbstractLearnerGraph.generateNewLabel("lbl/p",grBackToPairs.config,null),rejectState);
        grBackToPairs.addTransition(grBackToPairs.transitionMatrix.get(DeterministicDirectedSparseGraph.VertexID.parseID("a")),
                AbstractLearnerGraph.generateNewLabel("lbl/t",grBackToPairs.config,null),rejectState);
        grBackToPairs.addTransition(grBackToPairs.transitionMatrix.get(DeterministicDirectedSparseGraph.VertexID.parseID("c")),
                AbstractLearnerGraph.generateNewLabel("lbl/g",grBackToPairs.config,null),rejectState);

        Assert.assertNull(WMethod.checkM(grBackToPairs, grBackToPairs.getInit(),gr,gr.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));

        List<String> outputsA = new ArrayList<>();
        LearnerGraph grM_int = grM.transform.numberOutputsAndStates(true,outputsA,null, null);
        List<String> outputsB = new ArrayList<>();
        LearnerGraph grConf_int = grConf.transform.numberOutputsAndStates(true,outputsB,null,null);
        Assert.assertEquals(Arrays.asList("g","k","p"), outputsA);
        Assert.assertEquals(outputsA, outputsB);
        Assert.assertNull(WMethod.checkM(grM_int, grM_int.getInit(),grConf_int,grConf_int.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete0() {
        final LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete1() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->A", "testConvertToIOPairsAndComplete1a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-{a,b}->A", "testConvertToIOPairsAndComplete1b", Configuration.getDefaultConfiguration().copy(), converter);
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete2() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C", "testConvertToIOPairsAndComplete2a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#P", "testConvertToIOPairsAndComplete2b", Configuration.getDefaultConfiguration().copy(), converter);
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete3() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#R", "testConvertToIOPairsAndComplete3a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#P", "testConvertToIOPairsAndComplete3b", Configuration.getDefaultConfiguration().copy(), converter);
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete4() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / B-{q,e}->C / A-{a,c}-#R / B-{q,b}-#R / A-{a,e}-#T", "testConvertToIOPairsAndComplete4a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#P / A-{a,e}-#R / B-{q,e}->C", "testConvertToIOPairsAndComplete4b", Configuration.getDefaultConfiguration().copy(), converter);
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testConvertToIOPairsAndComplete5() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / B-{q,e}->C / A-{a,c}-#R / B-{q,b}-#R / A-{a,e}-#T / A-{q,b}->A", "testConvertToIOPairsAndComplete5a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        LearnerGraph completed = fsm.transform.convertToIOPairsAndCompleteOutputs();

        final LearnerGraph expected = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / A-{q,b}->A / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#P / A-{a,e}-#R / B-{q,e}->C / A-{q,c}-#R / A-{q,e}-#R", "testConvertToIOPairsAndComplete5b", Configuration.getDefaultConfiguration().copy(), converter);
        expected.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertNull(WMethod.checkM(expected,expected.getInit(),completed, completed.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }

    @Test
    public final void testExportDot1() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / B-{q,e}->C / A-{a,c}-#R / B-{q,b}-#R / A-{a,e}-#T / A-{q,b}->A", "testConvertToIOPairsAndComplete5a", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        Assert.assertEquals("digraph testConvertToIOPairsAndComplete5a {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "\t\"A\" [shape=doublecircle];\n" +
                "\t\"B\" [shape=doublecircle];\n" +
                "\t\"C\" [shape=doublecircle];\n" +
                "\t\"R\" [shape=square];\n" +
                "\t\"T\" [shape=square];\n" +
                "\t\"A\"->\"B\" [label=\"a/b\"];\n" +
                "\t\"A\"->\"R\" [label=\"a/c\"];\n" +
                "\t\"A\"->\"T\" [label=\"a/e\"];\n" +
                "\t\"A\"->\"A\" [label=\"q/b\"];\n" +
                "\t\"B\"->\"R\" [label=\"q/b\"];\n" +
                "\t\"B\"->\"C\" [label=\"q/c\"];\n" +
                "\t\"B\"->\"C\" [label=\"q/e\"];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot2() {
        Transform.ConvertALabel converter = new Transform.StringToMealyLabelConverter();
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{a,c}->C", "testExportDot2", Configuration.getDefaultConfiguration().copy(), converter);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.BLUE);fsm.findVertex("C").setColour(JUConstants.AMBER);
        Assert.assertEquals("digraph testExportDot2 {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "\t\"A\" [shape=doublecircle,fillcolor=blue];\n" +
                "\t\"B\" [shape=doublecircle,fillcolor=red];\n" +
                "\t\"C\" [shape=doublecircle];\n" +
                "\t\"A\"->\"B\" [label=\"a/b\"];\n" +
                "\t\"B\"->\"C\" [label=\"a/c\"];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot3a() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B / B-b->C", "testExportDot3a", Configuration.getDefaultConfiguration().copy(), null);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.BLUE);fsm.findVertex("C").setColour(JUConstants.AMBER);
        Assert.assertEquals("digraph testExportDot3a {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "\t\"A\" [shape=doublecircle,fillcolor=blue];\n" +
                "\t\"B\" [shape=doublecircle,fillcolor=red];\n" +
                "\t\"C\" [shape=doublecircle];\n" +
                "\t\"A\"->\"B\" [label=\"a\"];\n" +
                "\t\"B\"->\"C\" [label=\"b\"];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot3b() {
        final LearnerGraph fsm = FsmParserStatechum.buildLearnerGraph("A-a->B / B-b->C", "testExportDot3b", Configuration.getDefaultConfiguration().copy(), null);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        fsm.setInit(fsm.findVertex("B"));
        fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.BLUE);fsm.findVertex("C").setColour(JUConstants.AMBER);
        Assert.assertEquals("digraph testExportDot3b {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "\t\"B\" [shape=doublecircle,fillcolor=red];\n" +
                "\t\"A\" [shape=doublecircle,fillcolor=blue];\n" +
                "\t\"C\" [shape=doublecircle];\n" +
                "\t\"A\"->\"B\" [label=\"a\"];\n" +
                "\t\"B\"->\"C\" [label=\"b\"];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot3c() {
        final LearnerGraphND fsm = FsmParserStatechum.buildLearnerGraphND("A-a->B / A-a->C", "testExportDot3c", Configuration.getDefaultConfiguration().copy(), null);
        fsm.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);
        fsm.setInit(fsm.findVertex("B"));
        fsm.findVertex("B").setColour(JUConstants.RED);fsm.findVertex("A").setColour(JUConstants.BLUE);fsm.findVertex("C").setColour(JUConstants.AMBER);
        Assert.assertEquals("digraph testExportDot3c {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "\t\"B\" [shape=doublecircle,fillcolor=red];\n" +
                "\t\"A\" [shape=doublecircle,fillcolor=blue];\n" +
                "\t\"C\" [shape=doublecircle];\n" +
                "\t\"A\"->\"B\" [label=\"a\"];\n" +
                "\t\"A\"->\"C\" [label=\"a\"];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot4() {
        final LearnerGraph fsm = new LearnerGraph(Configuration.getDefaultConfiguration().copy());
        fsm.initEmpty();
        Assert.assertEquals("digraph graph {\n" +
                "\trankdir=LR;\n" +
                "\tnode [shape=circle, style=filled, fillcolor=white];\n" +
                "}\n",OutputUtil.dotGraphMealy(fsm).toString());
    }

    @Test
    public final void testExportDot5() {
        final LearnerGraph referenceGraph = FsmParserStatechum.buildLearnerGraph("A-{a,b}->B / B-{q,c}->C / A-{a,c}-#R / B-{q,b}-#P / A-{a,e}-#R / B-{q,e}->C", "testConvertToIOPairsAndComplete4b", Configuration.getDefaultConfiguration().copy(), converter);
        referenceGraph.config.setLabelKind(Configuration.LABELKIND.LABEL_ATOMICPAIRS);

        LearnerGraph graph = FsmParserDot.buildLearnerGraph(OutputUtil.dotGraphMealy(referenceGraph).toString(),
                configLTS,null, false,FsmParserDot.HOW_TO_FIND_INITIAL_STATE.FIRST_FOUND);
        Assert.assertNull(WMethod.checkM(referenceGraph,referenceGraph.getInit(),graph, graph.getInit(), WMethod.VERTEX_COMPARISON_KIND.NONE,false));
    }
}

