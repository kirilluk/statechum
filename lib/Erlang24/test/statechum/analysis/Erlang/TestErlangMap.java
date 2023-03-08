package statechum.analysis.Erlang;/* Copyright (c) 2023 The University of Sheffield
 *
 * This file is part of StateChum
 *
 * StateChum is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * StateChum is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
 */

import com.ericsson.otp.erlang.*;
import org.junit.*;

import java.util.Map;
import java.util.Set;

import static statechum.TestHelper.checkForCorrectException;
import static statechum.analysis.Erlang.TestErlangParser.checkResponse;


public class TestErlangMap
{
    /** Runner for the tests. */
    ErlangRunner runner;
    /** The runtime containing Erlang. */
    static ErlangRuntime runtime;

    @BeforeClass
    public static void beforeClass()
    {
        runtime = new ErlangRuntime();runtime.setTimeout(100);
        runtime.startRunner();
    }

    @AfterClass
    public static void afterClass()
    {
        runtime.killErlang();
    }

    @Before
    public void beforeTest()
    {
        runner = runtime.createNewRunner();
    }

    @After
    public void afterTest()
    {
        if (runner != null) runner.close();
    }

    @Test
    public void testParseMap1() {
        for (String text : new String[]{
                " #{ a =>5, b =>text}", " #{ a =>5, b =>text }", " #{a =>5, b =>text}", " #{a =>5,b =>text}",
                " #{ a => 5, b => text}", " #{ a => 5, b => text }", " #{a => 5, b => text}", " #{a => 5,b => text}"}) {
            Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangMap);
            checkResponse(runner, "#{'a' => 5,'b' => 'text'}", text);
        }
    }

    @Test
    public void testParseMap1Fail1()
    {
        final String text = "#{a";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail2()
    {
        final String text = "#{a #{";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting => in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail3()
    {
        final String text = "#{a{}";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting => in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail4()
    {
        final String text = "#{a,";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected token in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail5()
    {
        final String text = "#{a{ ";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"expecting => in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail6()
    {
        final String text = "#{a , ";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected token in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail7()
    {
        final String text = "#{a => ";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail8()
    {
        final String text = "#{,";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected token in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail9()
    {
        final String text = "#{ , ";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected token in parsing map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail10()
    {
        final String text = "#{ 56 => u, hh";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail11()
    {
        final String text = "#{ 56 => u, hh =>";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail12()
    {
        final String text = "#{ 56 => u, hh => tt";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap1Fail13()
    {
        final String text = "#{ 56 => u, hh => tt,";
        checkForCorrectException(() -> ErlangLabel.parseText(text),IllegalArgumentException.class,"unexpected end of map");
        checkForCorrectException(() -> runner.evaluateString(text),IllegalArgumentException.class,"badmatch");
    }

    @Test
    public void testParseMap2() {
        for (String text : new String[]{
                " #{ }", " #{}", "#{}", " #{ } "}) {
            Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangMap);
            checkResponse(runner, "#{}", text);
        }
    }

    @Test
    public void testParseMap3() {
        for (String text : new String[]{
                " #{ a => b }", " #{a => b}", "#{a =>b }", " #{  a => b} "}) {
            Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangMap);
            OtpErlangMap map = (OtpErlangMap) ErlangLabel.parseText(text);
            Set<Map.Entry<OtpErlangObject, OtpErlangObject>> entryset = map.entrySet();
            Assert.assertEquals(1, entryset.size());
            Map.Entry<OtpErlangObject, OtpErlangObject> entry = entryset.iterator().next();
            Assert.assertEquals(new OtpErlangAtom("a"), entry.getKey());
            Assert.assertEquals(new OtpErlangAtom("b"), entry.getValue());
            checkResponse(runner, "#{'a' => 'b'}", text);
        }
    }

    @Test
    public void testParseMap4() {
        for (String text : new String[]{
                " #{ 45.8 => 8, e => 267.5E40 }", " #{45.8 =>8, e=> 267.5E40 }", " #{45.8 => 8, e =>267.5E40}", " #{ 45.8 => 8, e => 267.5E40}"}) {
            Assert.assertTrue(ErlangLabel.parseText(text) instanceof OtpErlangMap);
            checkResponse(runner, "#{45.8 => 8,'e' => 2.675E42}", text);
        }
    }



    @Test
    public void testParseMapBig()
    {
        final String text = "#{'this is an atom' => \"this is a string\",[[-234],#{'a' => 'n',#{'c' => 'd'} => #{6 => 'l'}}] => ['another']}";
        OtpErlangObject obtained = ErlangLabel.parseText(text);
        checkResponse(runner, text, text);
        Assert.assertEquals(new OtpErlangMap(new OtpErlangObject[]{
                new OtpErlangAtom("this is an atom"),
                new OtpErlangList(new OtpErlangObject[]{
                        new OtpErlangList(new OtpErlangObject[]{
                                new OtpErlangInt(-234)
                        }),
                        new OtpErlangMap(new OtpErlangObject[]{
                                new OtpErlangMap(new OtpErlangObject[]{
                                        new OtpErlangAtom("c")}, new OtpErlangObject[]{new OtpErlangAtom("d") }),
                                new OtpErlangAtom("a")
                        },
                                new OtpErlangObject[]{
                                        new OtpErlangMap(new OtpErlangObject[]{
                                                new OtpErlangInt(6)}, new OtpErlangObject[]{new OtpErlangAtom("l") }),
                                        new OtpErlangAtom("n")
                                })
                })},
                new OtpErlangObject[]{
                        new OtpErlangString("this is a string"),
                        new OtpErlangList(new OtpErlangObject[]{
                                new OtpErlangAtom("another")
                        })
                }),obtained);
    }

}