package statechum.analysis.Erlang;

import org.junit.Assert;
import org.junit.Test;

import java.io.File;

import static statechum.TestHelper.checkForCorrectException;

public class TestGetName {
    @Test
    public void testGetErlName1()
    {
        checkValid("aa","aa.erl");
        checkValid("aa","  aa.erl   ");
    }

    @Test
    public void testGetErlName2()
    {
        checkValid("aa/bb","  aa/bb.erl   ");
        checkValid("aa/bb...","  aa/bb....erl   ");
        checkValid("aa/bb.c.d","  aa/bb.c.d.erl   ");
        checkValid("aa/bb.c.d.erl","  aa/bb.c.d.erl.erl   ");
    }

    void checkValid(String expected, String whatToPassToGetErlName)
    {
        Assert.assertEquals(expected,ErlangRunner.getErlName(whatToPassToGetErlName));Assert.assertTrue(ErlangRunner.validName(whatToPassToGetErlName));
    }

    void checkInvalid(String name)
    {
        Assert.assertNull(ErlangRunner.getErlName(name));Assert.assertFalse(ErlangRunner.validName(name));
    }

    @Test
    public void testGetErlNameFail()
    {
        checkInvalid(".erl");
        checkInvalid(null);
        checkInvalid("aa.berl");
        checkInvalid("aa.berl");
        checkInvalid("aa.erl.tt");
        checkInvalid("aa");
    }

    @Test
    public void testGetName1()
    {
        Assert.assertTrue(ErlangRunner.getName(new File("bb.c.d.erl.erl"), ErlangRunner.ERL.BEAM, true).endsWith("bb.c.d.erl.beam"));
        Assert.assertTrue(ErlangRunner.getName(new File("  bb....erl   "), ErlangRunner.ERL.BEAM, true).endsWith("bb....beam"));
    }

    @Test
    public void testGetNameFail()
    {
        checkForCorrectException(() -> ErlangRunner.getName(new File("  aa/bb....   "), ErlangRunner.ERL.BEAM, true),IllegalArgumentException.class,"Invalid module");
    }
}
