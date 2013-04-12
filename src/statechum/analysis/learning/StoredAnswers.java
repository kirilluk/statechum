/* Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
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

package statechum.analysis.learning;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import com.ericsson.otp.erlang.OtpErlangObject;

import statechum.Configuration;
import statechum.Label;
import statechum.Pair;
import statechum.apps.QSMTool;
import statechum.analysis.Erlang.ErlangLabel;
import statechum.analysis.learning.rpnicore.LTL_to_ba.Lexer;
import statechum.analysis.learning.rpnicore.Transform.ConvertALabel;

public class StoredAnswers implements AbstractOracle
{
	private final Configuration config;
	private final ConvertALabel converter;
	
	public StoredAnswers(Configuration conf, ConvertALabel conv)
	{
		config = conf;converter = conv;
	}
	
	private Map<List<Label>,Pair<Integer,String>> answers = new HashMap<List<Label>, Pair<Integer,String>>();
	
	protected void throwEx(String line)
	{
		throw new IllegalArgumentException("could not parse line : "+line);			
	}

	final Lexer lex = new Lexer(
			"(\\s*<yes>)|"+
			"(\\s*<no>\\s+at position +(.+),.*)|"+
			"(\\s*(<.+>)\\s*(.*))"// here I assume the second \\s* is greedy and will consume all spaces
			);

	final int 
		lexYES = 1,
		lexNO = 2, lexNO_position = 3, 
		lexOther = 4, lexResponse = 5,lexOther_text = 6;

	final Pattern usefulData = Pattern.compile("[ \\t]*("+RPNILearner.QUESTION_USER+").*");
	
	public synchronized void setAnswers(Reader src) throws IOException
	{
		BufferedReader reader = new BufferedReader(src);//new FileReader(src));
		String line = reader.readLine();
		while( line != null )
		{
			String trimmedLine = line.trim(); 
			if (trimmedLine.length() > 0 && usefulData.matcher(line).lookingAt())
			{// we are here if the line is non-empty and contains the magic keyword
				int tagEnd = trimmedLine.indexOf('>');
				if (tagEnd < 0)
					throwEx("missing end of tag \">\"");
				String tag = trimmedLine.substring(0, tagEnd+1);
				if (tag.equals(RPNILearner.QUESTION_USER))
				{
					String questionAndResponse = trimmedLine.substring(tagEnd+1);
					String theRestOfResponse = null;
					List<Label> question = null;
					{
				    	Lexer lexer = ErlangLabel.buildLexer(questionAndResponse);
				    	OtpErlangObject result = ErlangLabel.parseFirstTermInText(lexer);
				    	question = statechum.StatechumXML.readInputSequenceFromErlangObject(result,config,converter);
				    	if (lexer.getLastMatchType() < 0)
				    		throwEx("question "+questionAndResponse);
				    	theRestOfResponse = lexer.remaining();
					}
					lex.startParsing(theRestOfResponse);
					
					if (lex.getMatchType() < 0)
						throwEx("setAnswers: cannot parse "+theRestOfResponse);
	
					String errMsg = "unrecognised response "+lex.getMatch();
					switch(lex.getLastMatchType())
					{
					case lexYES:
						answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_ACCEPTED,null));
						break;
					case lexNO:
						answers.put(question, new Pair<Integer,String>(Integer.parseInt(lex.group(lexNO_position)),null));
						break;
					default:
						String response = lex.group(lexResponse);
						if (response.equals("<"+QSMTool.cmdLTL+">"))
						{
							if (lex.group(lexOther_text).isEmpty())
								throwEx(errMsg);
							answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_LTL,lex.group(lexOther_text)));				
						}
						else
						if (response.equals("<"+QSMTool.cmdIFTHENAUTOMATON+">"))
						{
							if (lex.group(lexOther_text).isEmpty())
								throwEx(errMsg);
							answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_IFTHEN,lex.group(lexOther_text)));				
						}
						else
						if (response.equals(RPNILearner.QUESTION_IGNORE))
						{
							if (!lex.group(lexOther_text).isEmpty())
								throwEx(errMsg);
							answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_IGNORED,null));				
						}
						else
						if (response.equals(RPNILearner.QUESTION_INCOMPATIBLE))
						{
							if (lex.group(lexOther_text).isEmpty())
								throwEx(errMsg);
							answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_INCOMPATIBLE,lex.group(lexOther_text)));				
						}
						else
						if (response.equals(RPNILearner.QUESTION_NEWTRACE))
						{
							if (lex.group(lexOther_text).isEmpty())
								throwEx(errMsg);
							answers.put(question, new Pair<Integer,String>(AbstractOracle.USER_NEWTRACE,lex.group(lexOther_text)));				
						}
						else
							throwEx(errMsg);
					}
				}
			}			
			line = reader.readLine();
		}
	}
	
	/** test method - returns the number of questions loaded. */
	public int getCount()
	{
		return answers.size();
	}
	
	/** Retrieves a stored answer. */
	@Override
	public Pair<Integer,String> getAnswer(List<Label> question)
	{
		Pair<Integer,String> result = null;
		if (answers != null && answers.containsKey(question))
			result = answers.get(question);
		
		return result;
	}
}
