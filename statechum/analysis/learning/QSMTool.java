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

import statechum.Configuration;
import statechum.analysis.learning.Visualiser.VIZ_ENV_PROPERTIES;

public class QSMTool {
	
	private static boolean includeLTL =false;
	private static int k = -1;

	public static void main(String[] args) {
		Set<List<String>> sPlus = new HashSet<List<String>>();
		Set<List<String>> sMinus = new HashSet<List<String>>();
		Set<String> ltl = new HashSet<String>();
		boolean active = true;
		try {
			BufferedReader in = new BufferedReader(new FileReader(args[0]));
			String fileString;
			String activePassive = in.readLine();
			if (activePassive.trim().equalsIgnoreCase("passive"))
				active = false;
			while ((fileString = in.readLine()) != null) {
				process(fileString, sPlus, sMinus, ltl);
			}
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		// new PickNegativesVisualiser(new
		// SootCallGraphOracle()).construct(sPlus, sMinus,null, active);
		Configuration config = Configuration.getDefaultConfiguration();
		String AutoName = System.getProperty(VIZ_ENV_PROPERTIES.VIZ_AUTOFILENAME.toString());
		if (AutoName != null) config.setAutoAnswerFileName(AutoName);
		PickNegativesVisualiser.setSimpleConfiguration(config, active, k);
		PickNegativesVisualiser pnv = new PickNegativesVisualiser();
		if (!includeLTL)
			pnv.construct(sPlus, sMinus, null, config);
		else
			pnv.construct(sPlus, sMinus, ltl, config);
		
		pnv.startLearner(null);
	}

	private static void process(String fileString, Set<List<String>> sPlus,
			Set<List<String>> sMinus, Set<String> ltl) {
		if (fileString.trim().equalsIgnoreCase(""))
			return;
		StringTokenizer tokenizer = new StringTokenizer(fileString.substring(1));
		ArrayList<String> sequence = new ArrayList<String>();
		while (tokenizer.hasMoreTokens())
			sequence.add(tokenizer.nextToken());
		if (fileString.startsWith("+"))
			sPlus.add(sequence);
		else if (fileString.startsWith("-"))
			sMinus.add(sequence);
		else if (fileString.startsWith("ltl")){
			includeLTL = true;
			if(fileString.substring(4).trim().length()>0)
				ltl.add(getLtlString(sequence));
			
		}
		else if (fileString.startsWith("k")){
			String value = fileString.substring(1).trim();
			k = Integer.valueOf(value);
		}

	}

	private static String getLtlString(List<String> sequence) {
		String expression = new String();
		for (int i = 1; i < sequence.size(); i++) {
			expression = expression.concat(sequence.get(i));
		}
		return expression;
	}

}
