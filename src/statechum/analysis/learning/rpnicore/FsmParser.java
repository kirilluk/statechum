package statechum.analysis.learning.rpnicore;

import java.util.regex.Matcher;

/**
 * Parent class for different parsers that make it possible to load automata.
 */
public class FsmParser
{
    protected final String text;
    protected Matcher lexer = null;
    protected String lastMatch = null;

    public FsmParser(String whatToParse) {
        text = "\n"+whatToParse;
    }

    protected boolean isFinished() {
        return lexer.regionStart() == lexer.regionEnd();
    }

    protected void throwException(String errMsg) {
        throw new IllegalArgumentException(errMsg + " starting from " + text.substring(lexer.regionStart()));
    }

    protected int getNextMatch() {
        if (!lexer.lookingAt())
            throwException("failed to lex");

        int i = 1;
        for (; i < lexer.groupCount() + 1 && lexer.group(i) == null; ++i);

        if (i == lexer.groupCount() + 1)
            throwException("failed to lex (group number is out of boundary)");

        lastMatch = lexer.group(i).trim();
        lexer.region(lexer.end(i), lexer.regionEnd());
        return i - 1;// to bring it to 0..max from 1..max+1
    }

    protected String getMatch() {
        return lastMatch;
    }
}
