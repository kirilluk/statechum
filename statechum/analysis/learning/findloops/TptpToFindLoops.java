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

package statechum.analysis.learning.findloops;

import statechum.analysis.learning.profileStringExtractor.*;

import java.io.*;
import javax.xml.parsers.*;

import javax.xml.parsers.SAXParserFactory;


/**
 * This class is responsible for taking a series of tptp profile sequences and converting them to 
 * observations that can be read by findloops (Jourdan et al, Forte 2007).
 * @author Neil Walkinshaw
 *
 */

public class TptpToFindLoops {
	
	/**
	 * @param args
	 * first argument is the name of the tptp trace file (decompressed)
	 * second argument is the name of the findloops output file
	 */
	public static void main (String[] args){
		File tptpFile = new File(args[0]);
		File[] extractorArray = new File[1];
		extractorArray[0]=tptpFile;
		Extractor extractor = new Extractor(extractorArray);
		ClassMethodDefsHandler handler = extractor.getFileToHandler().get(tptpFile);
		TranslatorParser translator = new TranslatorParser(args[1],handler);
		SAXParserFactory factory = SAXParserFactory.newInstance();
		try{
			SAXParser parser = factory.newSAXParser();
			parser.parse(tptpFile, translator);
		}
		catch(Exception e){
			e.printStackTrace();
		}
	}
	
}
