/*Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 
This file is part of StateChum

StateChum is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

StateChum is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with StateChum.  If not, see <http://www.gnu.org/licenses/>.
*/ 

package statechum.analysis.learning;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import statechum.Pair;
import statechum.apps.QSMTool;


public class StoredAnswers implements AbstractOracle
{
	private Map<String,Pair<Integer,String>> answers = new HashMap<String, Pair<Integer,String>>();
	
	protected void throwEx(String line)
	{
		throw new IllegalArgumentException("could not parse line "+line);			
	}

	public synchronized void setAnswers(Reader src) throws IOException
	{
		final int GROUP_TEXT = 2, GROUP_YES = 4, GROUP_NO = 5, GROUP_NO_NUM = 6, 
			GROUP_LTL = 7, GROUP_LTL_CONSTRAINT = 8, 
			GROUP_IFTHEN = 9,GROUP_IFTHEN_CONSTRAINT = 10,
			GROUP_IGNORE = 11, GROUP_IGNORE_CONSTRAINT = 12, 
			GROUP_INCOMPATIBLE = 13, GROUP_INCOMPATIBLE_CONSTRAINT = 14, 
			GROUP_NEWTRACE = 15, GROUP_NEWTRACE_CONSTRAINT = 16;

		final Pattern pat = Pattern.compile("[ \\t]*("+RPNILearner.QUESTION_USER+") *\\0133([^\\0135]+)\\0135 +((<yes>.*)|(<no> +at position +(.+),.*)|"+
					"(<"+QSMTool.cmdLTL+"> +(.*))|(<"+QSMTool.cmdIFTHENAUTOMATON+"> +(.*))|"+
					"("+RPNILearner.QUESTION_IGNORE+"(.*))|"+
					"("+RPNILearner.QUESTION_INCOMPATIBLE+" +(.*))|"+
					"("+RPNILearner.QUESTION_NEWTRACE+" +(.*))"+
					")");
		final Pattern usefulData = Pattern.compile("[ \\t]*("+RPNILearner.QUESTION_USER+").*");
		BufferedReader reader = new BufferedReader(src);//new FileReader(src));
		String line = reader.readLine();
		while( line != null )
		{
			if (line.trim().length() > 0 && usefulData.matcher(line).lookingAt())
			{// we are here if the line is non-empty and contains the magic keyword
				Matcher lexer = pat.matcher(line);
				if (!lexer.lookingAt() || lexer.group(GROUP_TEXT) == null)
					throwEx(line);
				//for(int i=1;i<=lexer.groupCount();++i)
				//	System.out.println("("+i+") "+lexer.group(i));
				String text = "["+lexer.group(GROUP_TEXT)+"]";
				if (lexer.group(GROUP_YES) != null)
				{
					if (lexer.group(GROUP_NO) != null || lexer.group(GROUP_NO_NUM) != null || 
							lexer.group(GROUP_LTL) != null || lexer.group(GROUP_LTL_CONSTRAINT) != null 
							|| lexer.group(GROUP_IFTHEN) != null || lexer.group(GROUP_IFTHEN_CONSTRAINT) != null)
						throwEx(line);
					
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_ACCEPTED,null));
				}
				else
				if (lexer.group(GROUP_NO) != null)
				{
					if (lexer.group(GROUP_LTL) != null || lexer.group(GROUP_LTL_CONSTRAINT) != null 
							|| lexer.group(GROUP_IFTHEN) != null || lexer.group(GROUP_IFTHEN_CONSTRAINT) != null)
						throwEx(line);

					answers.put(text, new Pair<Integer,String>(Integer.parseInt(lexer.group(GROUP_NO_NUM)),null));				
				}
				else
				if (lexer.group(GROUP_LTL) != null)
				{
					if (lexer.group(GROUP_LTL_CONSTRAINT) == null || lexer.group(GROUP_LTL_CONSTRAINT).length() == 0 
							|| lexer.group(GROUP_IFTHEN) != null || lexer.group(GROUP_IFTHEN_CONSTRAINT) != null)
						throwEx(line);
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_LTL,lexer.group(GROUP_LTL_CONSTRAINT)));				
				}
				else
				if (lexer.group(GROUP_IFTHEN) != null)
				{
					if (lexer.group(GROUP_IFTHEN_CONSTRAINT) == null || lexer.group(GROUP_IFTHEN_CONSTRAINT).length() == 0)
						throwEx(line);
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_IFTHEN,lexer.group(GROUP_IFTHEN_CONSTRAINT)));				
				}
				else
				if (lexer.group(GROUP_IGNORE) != null)
				{
					if (lexer.group(GROUP_IGNORE_CONSTRAINT) == null || lexer.group(GROUP_IGNORE_CONSTRAINT).trim().length() != 0)
						throwEx(line);
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_IGNORED,null));				
				}
				else
				if (lexer.group(GROUP_INCOMPATIBLE) != null)
				{
					if (lexer.group(GROUP_INCOMPATIBLE_CONSTRAINT) == null || lexer.group(GROUP_INCOMPATIBLE_CONSTRAINT).length() == 0)
						throwEx(line);
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_INCOMPATIBLE,lexer.group(GROUP_INCOMPATIBLE_CONSTRAINT)));				
				}
				else
				if (lexer.group(GROUP_NEWTRACE) != null)
				{
					if (lexer.group(GROUP_NEWTRACE_CONSTRAINT) == null || lexer.group(GROUP_NEWTRACE_CONSTRAINT).length() == 0)
						throwEx(line);
					answers.put(text, new Pair<Integer,String>(AbstractOracle.USER_NEWTRACE,lexer.group(GROUP_NEWTRACE_CONSTRAINT)));				
				}
				else
					
					throwEx(line);
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
	public Pair<Integer,String> getAnswer(List<String> question)
	{
		Pair<Integer,String> result = null;
		String q = question.toString();
		if (answers != null && answers.containsKey(q))
			result = answers.get(q);
		
		return result;
	}
}
