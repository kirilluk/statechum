/* Copyright (c) 2023 The University of Sheffield
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
package statechum.analysis.Erlang;

import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import statechum.analysis.learning.rpnicore.LTL_to_ba;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static statechum.analysis.Erlang.ErlangLabel.*;
import static statechum.analysis.Erlang.ErlangMap.ParseState.EXPECT_SEPARATOR;

public class ErlangMap implements ErlangParserComponent {
    private static final ErlangMap singleton = new ErlangMap();

    public static ErlangMap getSingleton() {
        return singleton;
    }

    @Override
    public void dump(OtpErlangObject arg, StringBuffer resultHolder) {
        OtpErlangMap tuple = (OtpErlangMap) arg;

        resultHolder.append("#{");
        boolean first = true;
        for (Map.Entry<OtpErlangObject, OtpErlangObject> pair : tuple.entrySet()) {
            if (!first)
                resultHolder.append(',');
            else
                first = false;
            classToDumper.get(pair.getKey().getClass()).dump(pair.getKey(), resultHolder);
            resultHolder.append(" => ");
            classToDumper.get(pair.getValue().getClass()).dump(pair.getValue(), resultHolder);
        }
        resultHolder.append("}");
    }
    enum ParseState {
        EXPECT_FIRST("first element of pair"), EXPECT_SEPARATOR("=>"), EXPECT_SECOND("second element of pair"), EXPECT_COMMA(",");
        private final String token;

        public String getToken() {
            return token;
        }

        ParseState(String tok) {
            token = tok;
        }
    }

    @Override
    public OtpErlangObject parseObject(LTL_to_ba.Lexer lexer) {
        assert lexer.getLastMatchType() == erlMapBegin;

        List<OtpErlangObject> mapKeys = new ArrayList<>(), mapValues = new ArrayList<>();

        OtpErlangObject firstElementOfPair = null;
        // Parser state.
        ParseState parseState = ParseState.EXPECT_FIRST;
        boolean pullNextToken = true;// this will normally be true - it is false where we called ourselves recursively to
        // parse a chunk of data and the parser stopped at the end of that data. This means we need that ending token
        // to continue parsing thus should not grab another token (effectively discarding the current one).
        int currentMatch = lexer.getMatchType();
        while (currentMatch != erlTupleEnd) {// parsing ErlangMap that is expected to end via erlTupleEnd
            if (currentMatch < 0)
                throw new IllegalArgumentException(
                        "unexpected end of map");

            switch (currentMatch) {
                case erlMapSep:
                    if (parseState != EXPECT_SEPARATOR)
                        throw new IllegalArgumentException(
                                "expecting => in parsing map, looking at "
                                        + lexer.getMatch());
                    parseState = ParseState.EXPECT_SECOND;
                    break;
                case erlMapBegin:
                case erlTupleBegin:
                case erlListBegin:
                case erlAtomQuote:
                case erlBitStrBegin:
                case erlString:
                    if (parseState == ParseState.EXPECT_FIRST) {
                        //noinspection ConstantConditions
                        assert firstElementOfPair == null;
                        firstElementOfPair = tokenToParser.get(currentMatch).parseObject(lexer);
                        parseState = EXPECT_SEPARATOR;
                    } else
                    if (parseState == ParseState.EXPECT_SECOND) {
                        assert firstElementOfPair != null;
                        mapKeys.add(firstElementOfPair);mapValues.add(tokenToParser.get(currentMatch).parseObject(lexer));
                        parseState = ParseState.EXPECT_COMMA;firstElementOfPair = null;
                    } else
                        throw new IllegalArgumentException(
                                "expecting "+parseState.getToken()+" in parsing map, looking at "
                                        + lexer.getMatch());
                    break;
                case erlPositiveNumber:
                case erlNegativeNumber:
                case erlDot:
                case erlE:
                case erlText:
                    if (parseState == ParseState.EXPECT_FIRST) {
                        //noinspection ConstantConditions
                        assert firstElementOfPair == null;
                        firstElementOfPair = tokenToParser.get(currentMatch).parseObject(lexer);
                        parseState = EXPECT_SEPARATOR;
                    } else
                    if (parseState == ParseState.EXPECT_SECOND) {
                        assert firstElementOfPair != null;
                        mapKeys.add(firstElementOfPair);mapValues.add(tokenToParser.get(currentMatch).parseObject(lexer));
                        parseState = ParseState.EXPECT_COMMA;firstElementOfPair = null;
                    } else
                        throw new IllegalArgumentException(
                                "expecting "+parseState.getToken()+" in parsing map, looking at "
                                        + lexer.getMatch());

                    pullNextToken = false;
                    break;
                case erlComma:
                    if (parseState != ParseState.EXPECT_COMMA)
                        throw new IllegalArgumentException(
                                "unexpected token in parsing map, looking at "
                                        + lexer.getMatch());
                    parseState = ParseState.EXPECT_FIRST;
                    break;
                default:
                    throw new IllegalArgumentException("invalid token type "
                            + currentMatch + " in parsing map, looking at "
                            + lexer.getMatch()+ " so far parsed: "+mapKeys+" keys");
            }
            if (pullNextToken)
                currentMatch = lexer.getMatchType();
            else {// use the last token but pull the next one next time
                // 'round
                currentMatch = lexer.getLastMatchType();
                pullNextToken = true;
            }
        }
        return new OtpErlangMap(mapKeys.toArray(new OtpErlangObject[0]), mapValues.toArray(new OtpErlangObject[0]));
    }

}
