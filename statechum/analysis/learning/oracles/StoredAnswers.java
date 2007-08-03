package statechum.analysis.learning.oracles;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import statechum.analysis.learning.RPNIBlueFringeLearner;

public class StoredAnswers implements AbstractOracle
{
	private Map<String,Integer> answers = new HashMap<String, Integer>();
	
	protected void throwEx(String line)
	{
		throw new IllegalArgumentException("could not parse line "+line);			
	}

	public synchronized void setAnswers(Reader src) throws IOException
	{
		final int GROUP_TEXT = 2, GROUP_YES = 4, GROUP_NO = 5, GROUP_NO_NUM = 6;

		final Pattern pat = Pattern.compile("[ \\t]*("+RPNIBlueFringeLearner.QUESTION_AUTO+")* *\\0133([^\\0135]+)\\0135 +((<yes>.*)|(<no> +at position +(.+),.*))");
		BufferedReader reader = new BufferedReader(src);//new FileReader(src));
		String line = reader.readLine();
		while( line != null )
		{
			if (line.trim().length() > 0)
			{
				Matcher lexer = pat.matcher(line);
				if (!lexer.lookingAt() || lexer.group(GROUP_TEXT) == null)
					throwEx(line);
				//for(int i=1;i<=lexer.groupCount();++i)
				//	System.out.println("("+i+") "+lexer.group(i));
				String text = "["+lexer.group(GROUP_TEXT)+"]";
				if (lexer.group(GROUP_YES) != null)
				{
					if (lexer.group(GROUP_NO) != null || lexer.group(GROUP_NO_NUM) != null)
						throwEx(line);
					
					answers.put(text, RPNIBlueFringeLearner.USER_ACCEPTED);
				}
				else
				{
					if (lexer.group(GROUP_NO) == null)
						throwEx(line);
					answers.put(text, Integer.parseInt(lexer.group(GROUP_NO_NUM)));				
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
	public int getAnswer(List<String> question)
	{
		int result = RPNIBlueFringeLearner.USER_CANCELLED;
		String q = question.toString();
		if (answers != null && answers.containsKey(q))
			result = answers.get(q);
		
		return result;
	}
}
