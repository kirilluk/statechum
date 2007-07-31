package statechum.analysis.learning;

/**
 * Takes a text file, structured as follows:
 * 
 * first line: either "active" or "passive" followed by \n
 * following lines:
 * strings that belong to the target machine:
 * + function1, function2...
 * + function1, function3...
 * and optionally strings that do NOT belong to the target machine:
 * -function1, function4
 * @author nw
 *
 */

import java.io.*;
import java.util.*;


public class QSMTool {
	
	private static Set<List<String>> sPlus, sMinus;
	
	public static void main(String[] args){
		sPlus = new HashSet<List<String>>();
		sMinus = new HashSet<List<String>>();
		boolean active = true;
		try{
			BufferedReader in = new BufferedReader(new FileReader(args[0]));
			String fileString;
			String activePassive = in.readLine();
			if(activePassive.equalsIgnoreCase("passive"))
				active = false;
	        while ((fileString = in.readLine()) != null) {
	            process(fileString);
	        }
	        in.close();
		} 	catch (IOException e) {e.printStackTrace();}
		new PickNegativesVisualiser().construct(sPlus, sMinus,null, active);
	}
	
	private static void process(String fileString){
		StringTokenizer tokenizer = new StringTokenizer(fileString.substring(1));
		ArrayList<String> sequence = new ArrayList<String>();
		while(tokenizer.hasMoreTokens())
			sequence.add(tokenizer.nextToken());
		if(fileString.startsWith("+"))
			sPlus.add(sequence);
		else
			sMinus.add(sequence);
			
	}

}
